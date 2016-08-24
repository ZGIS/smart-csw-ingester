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

package services

import java.util.concurrent.TimeoutException
import javax.inject.{Inject, Singleton}

import models.GmdElementSet
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{MatchAllDocsQuery, SearcherManager}
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.apache.lucene.store.RAMDirectory
import org.locationtech.spatial4j.context.SpatialContext
import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WSClient
import utils.ClassnameLogger
import utils.csw.{CswGetRecordsRequest, CswGetRecordsResponse}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

// explanation see https://www.playframework.com/documentation/2.5.x/ScalaJsonAutomated and
// https://www.playframework.com/documentation/2.5.x/ScalaJsonCombinators

case class SearchResultHeader(noDocuments: Int, query: String)

case class SearchResult(header: SearchResultHeader, results: List[GmdElementSet])

trait IndexService {
  def buildIndex(): Unit

  def query(query: String): SearchResult
}

/**
  * This service wraps around Lucene and controls all the CSW reading.
  *
  * @param appLifecycle  injected [[play.api.inject.ApplicationLifecycle]]
  * @param wsClient      injected [[play.api.libs.ws.WSClient]]
  * @param configuration injected [[play.api.Configuration]]
  */
@Singleton
class LuceneService @Inject()(appLifecycle: ApplicationLifecycle,
                              wsClient: WSClient,
                              configuration: Configuration)
  extends IndexService with ClassnameLogger {
  logger.info("Starting Lucene Service")

  logger.debug("Reading config: csw.catalogues ")
  //get Object List gives a Java-List and not a Scala List, so we convert here.
  private val cataloguesConfig = configuration.getConfigList("csw.catalogues").get.asScala.toList
  //this creates a list of tuples and converts them into a map
  val catalogues = cataloguesConfig.map { item => item.getString("name").get -> item.getString("url").get }.toMap
  val maxDocsPerFetch = configuration.getInt("csw.maxDocs").getOrElse(500)
  logger.error(s"max docs $maxDocsPerFetch")

  //stores the search index in RAM
  val directory = new RAMDirectory()

  buildIndex()
  //FIXME SR without an index we can't create the searcherManager. How to deal with that?
  //val searcherManager = new SearcherManager(directory, null)

  appLifecycle.addStopHook { () =>
    Future.successful(() => {
      logger.info("Stopping Lucene Service")
      directory.close()
    })
  }

  /**
    * Builds the Search index
    */
  //FIXME SR use something like lib-lucene-sugar. Lib unfortunately seems very old/outdated? https://github.com/gilt/lib-lucene-sugar
  def buildIndex(): Unit = {
    logger.info("Building Lucene Index")

    val gmdElemSetsFutures = Future.sequence(catalogues.map {
      case (catName, url) => {
        queryCatalogue(catName, url)
      }
    }.toList)

    val gmdElemSets = Await.result(gmdElemSetsFutures, 5 minutes).flatten
    logger.info(f"Loaded ${gmdElemSets.size} documents from CSW.")

    val analyzer = new StandardAnalyzer()
    val config = new IndexWriterConfig(analyzer)
    config.setCommitOnClose(true)
    //FIXME SR use SCALA_ARM (automated resource management)?
    val iwriter = new IndexWriter(directory, config)
    try {
      iwriter.deleteAll()
      iwriter.commit()
      gmdElemSets.foreach(searchDocument => iwriter.addDocument(searchDocument.asLuceneDocument()))

      iwriter.commit()
      logger.info("Index ready")
    }
    finally {
      iwriter.close()
    }
  }

  /**
    * Sends a GetRecords POST request to a catalogue and returns the response as a future.
    *
    * @param url           String containing URL to catalogue
    * @param startPosition Int with the first element to grab (pagination)
    * @param maxDocuments  Int with the max number of documents to return
    */
  //TODO SR maybe we should also return the exception object to give more meaningful error messages in the GUI?
  private def postGetRecordsRequest(url: String, startPosition: Int,
                                    maxDocuments: Int,
                                    f: Future[List[CswGetRecordsResponse]]): Future[List[CswGetRecordsResponse]] = {
    logger.info(f"Sending GetRecords request to $url")

    val wsClientResponseFuture =
      wsClient.url(url)
        .withRequestTimeout(20.seconds)
        .withHeaders("Content-Type" -> "application/xml")
        .post(CswGetRecordsRequest(startPosition, maxDocuments))

    val cswGetRecordsResponseListFuture = wsClientResponseFuture.flatMap { wsClientResponse =>
      logger.info(f"Response status: ${wsClientResponse.status} - ${wsClientResponse.statusText} ($url)")
      wsClientResponse.status match {
        case 200 => {
          logger.debug(f"Response Content Type: ${wsClientResponse.allHeaders.getOrElse("Content-Type", "Unknown")}")
          logger.debug(f"Response-Length: ${wsClientResponse.body.length}")
          logger.trace(f"Response-Body: ${wsClientResponse.body.toString}")
          wsClientResponse.xml.label match {
            case "ExceptionReport" => {
              logger.warn(
                f"Got Exception Response. Text: ${(wsClientResponse.xml \ "Exception" \ "ExceptionText").text}")
              Future.successful(List())
            }
            case "GetRecordsResponse" => {
              val cswGetRecResp = CswGetRecordsResponse(wsClientResponse.xml)
              logger.info(f"nextRecord: ${cswGetRecResp.nextRecord}, numberOfRec ${cswGetRecResp.numberOfRecordsMatched}")
              if ((cswGetRecResp.nextRecord > cswGetRecResp.numberOfRecordsMatched) ||
                (cswGetRecResp.nextRecord == 0))
                f.flatMap(l => {
                  Future.successful(cswGetRecResp :: l)
                })
              else
                f.flatMap(l => {
                  postGetRecordsRequest(url, cswGetRecResp.nextRecord, maxDocuments,
                    Future.successful(cswGetRecResp :: l))
                })
            }
            case _ => {
              logger.warn(f"Unknown response content. Body: ${wsClientResponse.xml.toString}")
              Future.successful(List())
            }
          }
        }
        case _ => {
          logger.warn(f"Error while executing CSW query.")
          logger.debug(f"Response body: ${wsClientResponse.body}")
          Future.successful(List())
        }
      }
    } recover {
        case e => {
          logger.warn(f"Exception on $url", e)
          List()
        }
    }
    cswGetRecordsResponseListFuture
  }

  /**
    *
    * @param catalogueName
    * @param url
    * @return
    */
  private def queryCatalogue(catalogueName: String, url: String): Future[List[GmdElementSet]] = {
    val getRecordsFuture = postGetRecordsRequest(url, 1, maxDocsPerFetch, Future.successful(Nil))
    val gmdElementsFuture = getRecordsFuture.map(cswGetRecordsResponses => {
      cswGetRecordsResponses.flatMap(cswGetRecordsResponse => {
        (cswGetRecordsResponse.xml \\ "MD_Metadata").map(mdMetadataNode => {
          logger.debug(f"Preparing($catalogueName): ${(mdMetadataNode \ "fileIdentifier" \ "CharacterString").text}")
          logger.trace(mdMetadataNode.toString)
          GmdElementSet.fromXml(mdMetadataNode, catalogueName)
        })
      })
    })
    gmdElementsFuture
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
        //FIXME errorhandling
        parser.parse(query)
      }
    }

    val searcherManager = new SearcherManager(directory, null)

    searcherManager.maybeRefreshBlocking()
    val isearcher = searcherManager.acquire()

    //FIXME SR when index empty, the next
    val search = isearcher.search(luceneQuery, isearcher.getIndexReader.numDocs())
    val scoreDocs = search.scoreDocs

    val header = SearchResultHeader(search.totalHits, luceneQuery.toString())
    val results = scoreDocs.map(scoreDoc => {
      val doc = isearcher.doc(scoreDoc.doc)
      GmdElementSet.fromLuceneDoc(doc)
    })
    //FIXME SR use ARM --> possible mem leak
    searcherManager.release(isearcher)
    SearchResult(header, results.toList)
  }
}
