/*
 * Copyright (c) 2011-2017 Interfaculty Department of Geoinformatics, University of Salzburg (Z_GIS)
 *                       & Institute of Geological and Nuclear Sciences Limited (GNS Science)
 *                       in the SMART Aquifer Characterisation (SAC) programme funded by the New Zealand
 *                       Ministry of Business, Innovation and Employment (MBIE)
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
 *
 */

package services

import javax.inject.{Inject, Singleton}

import models.GmdElementSet
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Document, Field, LongPoint, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{MatchAllDocsQuery, SearcherManager}
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.apache.lucene.store.RAMDirectory
import org.locationtech.spatial4j.context.SpatialContext
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.duration._
import scala.concurrent.Future

// explanation see https://www.playframework.com/documentation/2.5.x/ScalaJsonAutomated and
// https://www.playframework.com/documentation/2.5.x/ScalaJsonCombinators

case class SearchResultHeader(noDocuments: Int, query: String)

case class SearchResult(header: SearchResultHeader, results: List[GmdElementSet])

trait IndexService {
  def refreshIndex() : Unit
  def query(query: String): SearchResult
}

@Singleton
class LuceneService @Inject()(appLifecycle: ApplicationLifecycle, ws: WSClient) extends IndexService {

  // TODO parameters startPosition="1" maxRecords="15"
  val XML_REQUEST: String =
    """<?xml version="1.0" encoding="UTF-8"?>
<csw:GetRecords
  xmlns:csw="http://www.opengis.net/cat/csw/2.0.2"
  xmlns:ogc="http://www.opengis.net/ogc"
  xmlns:gmd="http://www.isotc211.org/2005/gmd"
  service="CSW" version="2.0.2"
  resultType="results" startPosition="1" maxRecords="15"
  outputFormat="application/xml" outputSchema="http://www.isotc211.org/2005/gmd"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.opengis.net/cat/csw/2.0.2 http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd">

  <csw:Query typeNames="gmd:MD_Metadata">
    <csw:ElementSetName>full</csw:ElementSetName>
  </csw:Query>
</csw:GetRecords>
""".stripMargin

  Logger.info("Starting Lucene Service")

  //stores the search index in RAM
  val directory = new RAMDirectory()

  refreshIndex()
  //as long as there is no index, we cant have a searcher manager. Figure out how to do :-)

  val searcherManager = new SearcherManager(directory, null)

  appLifecycle.addStopHook { () =>
    Future.successful(() => {
      Logger.info("Stopping Lucene Service")
      directory.close()
    })
  }

  /**
    * Refreshes the Search index
    */
  //FIXME use something like lib-lucene-sugar. Lib unfortunately seems very old/outdated? https://github.com/gilt/lib-lucene-sugar
  def refreshIndex(): Unit = {
    Logger.info("Refreshing Lucene Index")
    val analyzer = new StandardAnalyzer()
    val config = new IndexWriterConfig(analyzer)
    config.setCommitOnClose(true) // Use true to match behavior of Lucene 4.x.

    //FIXME use SCALA_ARM (automted resource management)?
    val iwriter = new IndexWriter(directory, config)
    try {
      iwriter.deleteAll()
      iwriter.commit()
      queryCsw().map { gmdElementSets =>
        Logger.info(f"Loaded ${gmdElementSets.size} documents to index")
        gmdElementSets.foreach(searchDocument => iwriter.addDocument(searchDocument.asLuceneDocument()))

      }
      iwriter.commit()
      Logger.info(f"Indexing ready")
    }
    finally {
      iwriter.close()
    }
  }

  def queryCsw(): Future[List[GmdElementSet]] = {

    val future: Future[scala.xml.NodeSeq] = ws.url("http://data.linz.govt.nz/feeds/csw/csw")
      .withHeaders("Content-Type" -> "application/xml")
      .post(XML_REQUEST).map{
        response =>
          // Logger.debug(response.body)
          val xml1: scala.xml.NodeSeq = scala.xml.XML.load(response.body)
          // response.xml
          xml1
       }

    val gmdElementSets: Future[List[GmdElementSet]] = future.map { xml =>
      // val gmdList: List[GmdElementSet] = (xml \\ "GetRecordsResponse" \ "SearchResults" \ "MD_Metadata" ).map{
      val gmdList: List[GmdElementSet] = (xml \\ "MD_Metadata" ).map{
        node =>
          Logger.debug(f"Preparing ${node.size}")
          val gmdElem = GmdElementSet.fromXml(node, "linz")
          Logger.debug(f"Preparing ${gmdElem.fileIdentifier}")
          gmdElem
      }.toList
      gmdList
    }
    gmdElementSets
  }

  /**
    * Queries the Search Index
    *
    * @param query
    * @return
    */
  def query(query: String): SearchResult = {

    val ctx = SpatialContext.GEO

    val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(ctx, "bboxStrategy")
    // Query = LongPoint.newRangeQuery("dateStampCompare", localDate1.toEpochDay, localDate2.toEpochDay)

    val luceneQuery = query.trim() match {
      case "" => new MatchAllDocsQuery()
      case _ => {
        val parser = new QueryParser("catch_all", new StandardAnalyzer())
        //FIXME errorhanding
        parser.parse(query)
      }
    }

    val isearcher = searcherManager.acquire()
    val search = isearcher.search(luceneQuery, isearcher.getIndexReader().numDocs())
    val scoreDocs = search.scoreDocs

    val header = SearchResultHeader(search.totalHits, luceneQuery.toString())
    val results = scoreDocs.map(scoreDoc => {
      val doc = isearcher.doc(scoreDoc.doc)
      GmdElementSet.fromLuceneDoc(doc)
    })
    SearchResult(header, results.toList)
  }
}
