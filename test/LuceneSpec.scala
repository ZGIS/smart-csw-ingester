/*
 * Copyright (c) 2011-2017 Interfaculty Department of Geoinformatics, University of
 * Salzburg (Z_GIS) & Institute of Geological and Nuclear Sciences Limited (GNS Science)
 * in the SMART Aquifer Characterisation (SAC) programme funded by the New Zealand
 * Ministry of Business, Innovation and Employment (MBIE)
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
 */

import com.typesafe.config.ConfigFactory
import models.gmd.GeoJSONFeatureCollectionWriter
import org.scalatestplus.play._
import play.api.{Configuration, Logger, Mode}
import play.api.inject.DefaultApplicationLifecycle
import play.api.libs.json.Json
import play.api.mvc._
import play.api.routing.sird._
import play.api.test._
import play.core.server.{Server, ServerConfig}
import services.LuceneService

import scala.concurrent.Await
import scala.concurrent.duration._


/**
  * this does some magic :-)
  * https://www.playframework.com/documentation/2.5.x/ScalaTestingWebServiceClients
  */
trait WithLuceneService {
  def withLuceneService[T](block: LuceneService => T): T = {
    Server.withRouter(ServerConfig(port = Some(9999), mode = Mode.Test)) {
      case POST(p"/csw1") => Action {
        Results.Ok.sendResource("getRecordsTestResponse.xml")
      }
    } { implicit port =>
      WsTestClient.withClient({ client =>
        val configResource = getClass.getResource("catalogues.test.conf")
        val config = Configuration(ConfigFactory.parseURL(configResource))

        val appLifeCycle = new DefaultApplicationLifecycle()
        val service = new LuceneService(appLifeCycle, client, config)
        val result = block(service)
        Await.result(appLifeCycle.stop(), 10.seconds)
        result
      })
    }
  }
}

class LuceneSpec extends PlaySpec with WithLuceneService {

  "LuceneService " should {

    implicit val geoJSONFeatureCollectionWrite = GeoJSONFeatureCollectionWriter

    "cannot mirror query string" in {
      withLuceneService { service =>
        val result = service.query("*:*")
        val resultJson = Json.toJson(result)
        // result.header.query mustBe "*:*"
      }
    }
    "have Index with 4 documents" in {
      withLuceneService { service =>
        val result = service.query("*:*")
        result.size mustBe 4
      }
    }
    /* SR hier jetzt schön queries testen :-)
      [info]   org.apache.lucene.queryparser.classic.ParseException:
      Cannot parse 'dateStampText:*': '*' or '?' not allowed as first character in WildcardQuery
     */

    "find fileIdentifier" in {
      withLuceneService { service =>
        val result = service.query("fileIdentifier:\"23bdd7a3-fd21-daf1-7825-0d3bdc256f9d\"")
        result.size mustBe 1
      }
    }
    "find title" in {
      withLuceneService { service =>
        val result = service.query("title:\"NZ Primary Road Parcels\"")
        result.size mustBe 1
      }
    }
    "find abstrakt" in {
      withLuceneService { service =>
        val result = service.query("abstrakt:road parcel polygons")
        result.size mustBe 1
      }
    }
    /* Date range queries not yet possible (dates as long, blower and upper limit)
        the "field" for a date range query is "dateStampCompare"
        LongPoint.newRangeQuery("dateStampCompare", localDate1.toEpochDay, localDate2.toEpochDay)
     */
    "find dateStampText" in {
      withLuceneService { service =>
        val result = service.query("dateStampText:\"2015-04-08\"")
        result.size mustBe 1
      }
    }
    "find keywords" in {
      withLuceneService { service =>
        val result = service.query("keywords:\"unwanted organisms\"")
        result.size mustBe 2
      }
    }
    "find topicCategory" in {
      withLuceneService { service =>
        val result = service.query("topicCategory:biota")
        result.size mustBe 2
      }
    }
    "find contactName" in {
      withLuceneService { service =>
        val result = service.query("contactName:Omit")
        // case insensitive
        result.size mustBe 2
      }
    }
    "find contactOrg" in {
      withLuceneService { service =>
        val result = service.query("contactOrg:\"LINZ\"")
        result.size mustBe 2
      }
    }
    "find contactEmail" in {
      withLuceneService { service =>
        val result = service.query("contactEmail:info@linz.govt.nz")
        result.size mustBe 2
      }
    }
    "find license" in {
      withLuceneService { service =>
        val result = service.query("license:Creative Commons")
        result.size mustBe 4
      }
    }
    /* spatial bbox queries not yet possible (bbox and spatial loperations like Intersects, IsDisjointTo, IsEqualTo)
    the "field" for a geo query ia "bboxStrategy"
      val bboxT = ctx.getShapeFactory().rect(-180.0, 180.0, -90.0, 90.0)
      val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(ctx, "bboxStrategy")
      val luceneQuery6 = bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.IsEqualTo, bboxT))
   */
    "find bboxText" in {
      withLuceneService { service =>
        val result = service.query("bboxText:ENVELOPE")
        result.size mustBe 4
      }
    }
    "find origin" in {
      withLuceneService { service =>
        val result = service.query("origin:test1")
        result.size mustBe 4
      }
    }

  }

  /*  "LuceneSpecs with OneAppPerSuite trait " must {

      "provide a Lucene Index for GmdElementSet" in {

        val ctx = SpatialContext.GEO

        val catalogues = Map("linz" -> "http://data.linz.govt.nz/feeds/csw/csw",
          "mfe" -> "http://data.mfe.govt.nz/feeds/csw/csw",
          "geogovt" -> "http://geodata.govt.nz/geonetwork/srv/en/csw",
          "niwa" -> "http://dc.niwa.co.nz/niwa_dc/srv/eng/csw",
          "landcare" -> "http://lris.scinfo.org.nz/feeds/csw/csw",
          "doc" -> "http://geoportal.doc.govt.nz/geoportal/csw",
          "gns" -> "http://data.gns.cri.nz/metadata/srv/eng/csw")

        val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
        val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)

        val asResource2 = this.getClass().getResource("sac_rs_ewt_metadata.xml")
        val xml2: scala.xml.NodeSeq = scala.xml.XML.load(asResource2)

        val directory = new RAMDirectory()
        val analyzer = new StandardAnalyzer()
        val config = new IndexWriterConfig(analyzer)

        def queryCsw(key: String, value: String): Future[String] = {
          Future {
            f"$key + $value"
          }
        }

        val result = catalogues.map {
          case (csw, url) => {
            println(f"$csw + $url")
            val res: String = Await.result(queryCsw(url, csw), 120.seconds)
            res
          }
        }

        result.foreach(
          str => println(str)
        )

        val testList = List(
          MdMetadataSet.fromXml(xml1).get,
          MdMetadataSet.fromXml(xml2).get
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
          MdMetadataSet.fromLuceneDoc(doc)
        })

        results(0).fileIdentifier mustBe "23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"
        println(f"search4 result: ${results(0).toString()}")

        val luceneQuery5 = LongPoint.newRangeQuery("dateStampCompare", localDate1.toEpochDay, localDate1.toEpochDay)
        val search5 = isearcher.search(luceneQuery5, isearcher.getIndexReader().numDocs())
        val scoreDocs5 = search5.scoreDocs

        search5.totalHits mustBe 0

        val bboxS = ctx.getShapeFactory().rect(-176.176448433, 166.6899599, -47.1549297167, -34.4322590833)
        val bboxT = ctx.getShapeFactory().rect(-180.0, 180.0, -90.0, 90.0)
        val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(ctx, "bboxStrategy")

        val luceneQuery6 = bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.IsEqualTo, bboxS))
        val search6 = isearcher.search(luceneQuery6, isearcher.getIndexReader().numDocs())

        search6.totalHits mustBe 1

        val luceneQuery7 = bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.IsDisjointTo, bboxT))
        val search7 = isearcher.search(luceneQuery7, isearcher.getIndexReader().numDocs())

        search7.totalHits mustBe 0

        val luceneQuery8 = bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.Intersects, bboxT))
        val search8 = isearcher.search(luceneQuery8, isearcher.getIndexReader().numDocs())

        search8.totalHits mustBe 2

        directory.close()
      }
    }*/
}
