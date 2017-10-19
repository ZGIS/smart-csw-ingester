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

import java.time.LocalDate

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import models.gmd.GeoJSONFeatureCollectionWriter
import org.scalatestplus.play._
import play.api.inject.DefaultApplicationLifecycle
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc._
import play.api.routing.sird._
import play.api.test._
import play.api.{Configuration, Mode}
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
    val port = 9999
    Server.withRouter(ServerConfig(port = Some(port), mode = Mode.Test)) {
      case POST(p"/csw1") => Action {
        Results.Ok.sendResource("getRecordsTestResponse.xml")
      }
    } { implicit port =>
      WsTestClient.withClient({ client =>
        val configResource = getClass.getResource("catalogues.test.conf")
        val config = Configuration(ConfigFactory.parseURL(configResource))

        val injector = new GuiceApplicationBuilder()
          .configure(config)
          .injector()

        val appLifeCycle = new DefaultApplicationLifecycle()
        val service = injector.instanceOf[LuceneService]
        val actorSystem = injector.instanceOf[ActorSystem]
        // FIXME SR this is the dirtiest way possible to wait for the service to index everything, but it works for now
        Thread.sleep(3000)
        val result = block(service)
        Await.result(appLifeCycle.stop(), 10.seconds)
        result
      })
    }
  }
}

class LuceneServiceSpec extends PlaySpec with WithLuceneService {
  "LuceneService " should {

    implicit val geoJSONFeatureCollectionWrite = GeoJSONFeatureCollectionWriter

    "mirror query string" in {
      withLuceneService { service =>
        val result = service.query("*:*")
        //        val resultJson = Json.toJson(result.documents)
        //        result.luceneQuery.toString mustBe "*:*"
        result.luceneQuery.toString.startsWith("+*:* ") mustBe true
      }
    }

    "find 5 documents for *:* query" in {
      withLuceneService { service =>
        val result = service.query("*:*")
        result.documents.size mustBe 5
      }
    }

    /* SR hier jetzt schön queries testen :-)
      [info]   org.apache.lucene.queryparser.classic.ParseException:
      Cannot parse 'dateStampText:*': '*' or '?' not allowed as first character in WildcardQuery
*/
    "find 1 document for fileIdentifier:23bdd7a3-fd21-daf1-7825-0d3bdc256f9d" in {
      withLuceneService { service =>
        val result = service.query("fileIdentifier:\"23bdd7a3-fd21-daf1-7825-0d3bdc256f9d\"")
        result.documents.size mustBe 1
        result.documents.head.fileIdentifier mustBe "23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"
      }
    }
    "find 1 document for title:NZ Primary Road Parcels" in {
      withLuceneService { service =>
        val result = service.query("title:\"NZ Primary Road Parcels\"")
        result.documents.size mustBe 1
        result.documents.head.fileIdentifier mustBe "23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"
      }
    }
    "find 1 document for abstrakt:road parcel polygons" in {
      withLuceneService { service =>
        val result = service.query("abstrakt:road parcel polygons")
        result.documents.size mustBe 1
        result.documents.head.fileIdentifier mustBe "23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"
      }
    }
    //     Date range queries not yet possible (dates as long, blower and upper limit)
    //        the "field" for a date range query is "dateStampCompare"
    //        LongPoint.newRangeQuery("dateStampCompare", localDate1.toEpochDay, localDate2.toEpochDay)
    "find 1 document for dateStampText:2015-04-08" in {
      withLuceneService { service =>
        val result = service.query("dateStampText:\"2015-04-08\"")
        result.documents.size mustBe 1
        result.documents.head.fileIdentifier mustBe "294f127b-addb-24d8-0df1-f014032dcd02"
      }
    }
    "find 2 documents for keywords:unwanted organisms" in {
      withLuceneService { service =>
        val result = service.query("keywords:\"unwanted organisms\"")
        result.documents.size mustBe 2
        result.documents.map(_.fileIdentifier).contains("0326a6b1-c163-405b-a3c7-9136ab4f4807") mustBe true
        result.documents.map(_.fileIdentifier).contains("4A467BBC-E7BA-4A98-AD9E-58A781041913") mustBe true
      }
    }
    "find 2 documents for topicCategory:biota" in {
      withLuceneService { service =>
        val result = service.query("topicCategory:biota")
        result.documents.size mustBe 2
        result.documents.map(_.fileIdentifier).contains("0326a6b1-c163-405b-a3c7-9136ab4f4807") mustBe true
        result.documents.map(_.fileIdentifier).contains("4A467BBC-E7BA-4A98-AD9E-58A781041913") mustBe true
      }
    }
    "find 2 documents contactName:Omit" in {
      withLuceneService { service =>
        val result = service.query("contactName:Omit")
        // case insensitive
        result.documents.size mustBe 2
        result.documents.map(_.fileIdentifier).contains("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d") mustBe true
        result.documents.map(_.fileIdentifier).contains("294f127b-addb-24d8-0df1-f014032dcd02") mustBe true
      }
    }
    "find 2 documents for contactOrg:\"LINZ\"" in {
      withLuceneService { service =>
        val result = service.query("contactOrg:\"LINZ\"")
        result.documents.size mustBe 2
        result.documents.map(_.fileIdentifier).contains("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d") mustBe true
        result.documents.map(_.fileIdentifier).contains("294f127b-addb-24d8-0df1-f014032dcd02") mustBe true
      }
    }
    "find 2 documents contactEmail:info@linz.govt.nz" in {
      withLuceneService { service =>
        val result = service.query("contactEmail:info@linz.govt.nz")
        result.documents.size mustBe 2
        result.documents.map(_.fileIdentifier).contains("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d") mustBe true
        result.documents.map(_.fileIdentifier).contains("294f127b-addb-24d8-0df1-f014032dcd02") mustBe true
      }
    }
    "find 4 documents for license:Creative Commons" in {
      withLuceneService { service =>
        val result = service.query("license:Creative Commons")
        result.documents.size mustBe 4
      }
    }
    "find 5 documents in origin:test1" in {
      withLuceneService { service =>
        val result = service.query("origin:test1")
        result.documents.size mustBe 5
      }
    }

    "find correct entries for BBOX" should {
      "find 5 documents for [-180,180,90,-90] (WORLD)" in {
        withLuceneService { service =>
          val result = service.query("*:*", Some("ENVELOPE(-180, 180, 90,-90)"))
          result.documents.size mustBe 5
        }
      }

      // intersect vs within
      "find 5 documents for [168,179,-34,-47]" in {
        withLuceneService { service =>
          val result = service.query("*:*", Some("ENVELOPE(168, 179,-34,-47)"))
          result.documents.size mustBe 5
          result.documents.exists(_.fileIdentifier.equalsIgnoreCase("294f127b-addb-24d8-0df1-f014032dcd02")) mustBe true
        }
      }

      // intersect vs within
      "find 5 documents for [166,-176,-34,-48]" in {
        withLuceneService { service =>
          val result = service.query("*:*", Some("ENVELOPE(166,-176,-34,-48)"))
          result.documents.size mustBe 5
          result.documents.exists(
            md => md.fileIdentifier.equalsIgnoreCase("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d")) mustBe true
        }
      }

      "correctly sanatize bogus BBOX in query to WORLD (find 5 results)" in {
        withLuceneService { service =>
          val result = service.query("*:*", Some("BOGUSBOX"))
          result.documents.size mustBe 5
        }
      }
    }

    "find correct entries for date ranges" should {
      "find 2 documents for 2015-01-15" in {
        withLuceneService { service =>
          val fromDate = LocalDate.of(2015, 1, 15)
          val toDate = LocalDate.of(2015, 1, 15)
          val result = service.query("*:*", None, Some(fromDate), Some(toDate))
          result.documents.size mustBe 2
          result.documents.map(_.fileIdentifier).contains("0326a6b1-c163-405b-a3c7-9136ab4f4807") mustBe true
          result.documents.map(_.fileIdentifier).contains("4A467BBC-E7BA-4A98-AD9E-58A781041913") mustBe true
        }
      }

      "find 3 documents for 2015-01-15 to 2015-04-08" in {
        withLuceneService { service =>
          val fromDate = LocalDate.of(2015, 1, 15)
          val toDate = LocalDate.of(2015, 4, 8)
          val result = service.query("*:*", None, Some(fromDate), Some(toDate))
          result.documents.size mustBe 3
          result.documents.map(_.fileIdentifier).contains("0326a6b1-c163-405b-a3c7-9136ab4f4807") mustBe true
          result.documents.map(_.fileIdentifier).contains("4A467BBC-E7BA-4A98-AD9E-58A781041913") mustBe true
          result.documents.map(_.fileIdentifier).contains("294f127b-addb-24d8-0df1-f014032dcd02") mustBe true
        }
      }

      "find 2 documents for None to 2012-12-20" in {
        withLuceneService { service =>
          val toDate = LocalDate.of(2012, 12, 20)
          val result = service.query("*:*", None, None, Some(toDate))
          result.documents.size mustBe 2
          result.documents.exists(
            md => md.fileIdentifier.equalsIgnoreCase("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d")) mustBe true
          result.documents.exists(
            md => md.fileIdentifier.equalsIgnoreCase("https://data.mfe.govt.nz/table/2508-water-quality-parameters-in-coastal-and-estuarine-environments-2013/")) mustBe true
        }
      }

      "find 1 document for 2015-04-08 to None" in {
        withLuceneService { service =>
          val fromDate = LocalDate.of(2015, 4, 8)
          val result = service.query("*:*", None, Some(fromDate), None)
          result.documents.size mustBe 1
          result.documents.head.fileIdentifier mustBe "294f127b-addb-24d8-0df1-f014032dcd02"
        }
      }

      "correctly retrieve document with empty topicCategory" in {
        // this tests for the BUG from https://github.com/ZGIS/smart-csw-ingester/issues/20
        withLuceneService { service =>
          val result = service.query(
            "fileIdentifier:\"https\\://data.mfe.govt.nz/table/2508\\-water\\-quality\\-parameters\\-in\\-coastal\\-and\\-estuarine\\-environments\\-2013/\"",
            None, None, None)
          result.documents.size mustBe 1
          result.documents.head.fileIdentifier mustBe ("https://data.mfe.govt.nz/table/2508-water-quality-parameters-in-coastal-and-estuarine-environments-2013/")
          result.documents.head.topicCategories mustBe List()
        }
      }
    }
  }
}
