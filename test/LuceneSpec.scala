/*
 * copyright (c) 2015 Interfaculty Department of Geoinformatics - Z_GIS (http://www.zgis.at),
 *                     University of Salzburg (http://www.uni-salzburg.at) in the
 *                     New Zealand SMART Aquifer Characterisation Programme (http://www.smart-project.info)
 *                     funded by the Ministry of Business, Innovation and Employment (http://www.mbie.govt.nz/)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

import java.time.{Instant, LocalDate, Month, ZoneId}

import models.GmdElementSet
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import play.api.inject.guice.GuiceApplicationBuilder
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Document, Field, LongPoint, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{MatchAllDocsQuery, PointRangeQuery, SearcherManager, TermQuery, TermRangeQuery}
import org.apache.lucene.store.RAMDirectory
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.shape.Rectangle
import services.{SearchResultDocument, SearchResultHeader}

class LuceneSpec extends PlaySpec with OneAppPerSuite {

  // Override app if you need a Application with other than
  // default parameters.
  // val application: Application = GuiceApplicationBuilder().build()

  implicit override lazy val app = new GuiceApplicationBuilder().configure(Map("ehcacheplugin" -> "disabled")).build()

  "LuceneSpecs with OneAppPerSuite trait " must {

    "provide a Lucene Setup" in {

      val directory = new RAMDirectory()
      val analyzer = new StandardAnalyzer()
      val config = new IndexWriterConfig(analyzer)

      val testList = List(
        SearchResultDocument("Document 1", "Abstract 1", "dummy ctl"),
        SearchResultDocument("Document 2", "Abstract 2", "dummy ctl"),
        SearchResultDocument("Document 3", "Abstract 3", "other ctl"),
        SearchResultDocument("Document 4", "Abstract 4 is longer", "other ctl")
      )

      val iwriter = new IndexWriter(directory, config)

      try {
        iwriter.deleteAll()
        iwriter.commit()
        testList.foreach(searchDocument => iwriter.addDocument(searchDocument.asLuceneDocument()))
        iwriter.commit()
      }
      finally {
        iwriter.close()
      }

      val searcherManager = new SearcherManager(directory, null)
      val isearcher = searcherManager.acquire()
      val parser = new QueryParser("catch_all", new StandardAnalyzer())

      val query1 = ""
      val query2 = "title:\"Document 1\""
      val luceneQuery1 = new MatchAllDocsQuery()

      val luceneQuery2 = parser.parse(query2)

      val search1 = isearcher.search(luceneQuery1, isearcher.getIndexReader().numDocs())
      val scoreDocs1 = search1.scoreDocs

      val search2 = isearcher.search(luceneQuery2, isearcher.getIndexReader().numDocs())
      val scoreDocs2 = search2.scoreDocs

      search1.totalHits mustBe 4
      search2.totalHits mustBe 1

      directory.close()
    }

    "provide a Lucene Index for GmdElementSet" in {

      val ctx = SpatialContext.GEO

      val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
      val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)

      val asResource2 = this.getClass().getResource("sac_rs_ewt_metadata.xml")
      val xml2: scala.xml.NodeSeq = scala.xml.XML.load(asResource2)

      val directory = new RAMDirectory()
      val analyzer = new StandardAnalyzer()
      val config = new IndexWriterConfig(analyzer)

      val testList = List(
        GmdElementSet.fromXml(xml1),
        GmdElementSet.fromXml(xml2)
      )

      val iwriter = new IndexWriter(directory, config)

      try {
        iwriter.deleteAll()
        iwriter.commit()
        testList.foreach(searchDocument => iwriter.addDocument(searchDocument.asLuceneDocument()))
        iwriter.commit()
      }
      finally {
        iwriter.close()
      }

      val searcherManager = new SearcherManager(directory, null)
      val isearcher = searcherManager.acquire()
      val parser = new QueryParser("catch_all", new StandardAnalyzer())

      val query1 = ""
      val luceneQuery1 = new MatchAllDocsQuery()
      val search1 = isearcher.search(luceneQuery1, isearcher.getIndexReader().numDocs())
      val scoreDocs1 = search1.scoreDocs

      val query2 = "title:\"NZ Primary Road Parcels\""
      val luceneQuery2 = parser.parse(query2)
      val search2 = isearcher.search(luceneQuery2, isearcher.getIndexReader().numDocs())
      val scoreDocs2 = search2.scoreDocs

      val query3 = "contactOrg:\"LINZ\""
      val luceneQuery3 = parser.parse(query3)
      val search3 = isearcher.search(luceneQuery3, isearcher.getIndexReader().numDocs())
      val scoreDocs3 = search3.scoreDocs

      search1.totalHits mustBe 2
      search2.totalHits mustBe 1
      search3.totalHits mustBe 1

      val localDate1 = java.time.LocalDate.of(2012, Month.DECEMBER, 19)
      val localDate2 = java.time.LocalDate.of(2012, Month.DECEMBER, 21)

      val luceneQuery4 = LongPoint.newRangeQuery("dateStampCompare", localDate1.toEpochDay, localDate2.toEpochDay)
      val search4 = isearcher.search(luceneQuery4, isearcher.getIndexReader().numDocs())
      val scoreDocs4 = search4.scoreDocs

      search4.totalHits mustBe 1

      val results = scoreDocs4.map(scoreDoc => {
        val doc = isearcher.doc(scoreDoc.doc)

        GmdElementSet(
          fileIdentifier = doc.get("fileIdentifier"),
          dateStamp = GmdElementSet.dateFromString(doc.get("dateStampText")),
          title = doc.get("title"),
          abstrakt = doc.get("abstrakt"),
          keywords = doc.get("keywords").split(",").toList,
          topicCategory = doc.get("topicCategory").split(",").toList,
          contactName = doc.get("contactName"),
          contactOrg = doc.get("contactOrg"),
          contactEmail = doc.get("contactEmail"),
          license = doc.get("license"),
          bbox = GmdElementSet.bboxFromWkt(doc.get("bboxText")),
          origin = doc.get("origin")
        )
      })

      results(0).fileIdentifier mustBe "23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"
      println(results(0).toString())

      val luceneQuery5 = LongPoint.newRangeQuery("dateStampCompare", localDate1.toEpochDay, localDate1.toEpochDay)
      val search5 = isearcher.search(luceneQuery5, isearcher.getIndexReader().numDocs())
      val scoreDocs5 = search5.scoreDocs

      search5.totalHits mustBe 0

      directory.close()
    }
  }

}