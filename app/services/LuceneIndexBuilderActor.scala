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

import javax.inject.Inject

import akka.actor.{Actor, Props}
import models.csw.{CswGetRecordsRequest, CswGetRecordsResponse}
import models.gmd.MdMetadataSet
import org.apache.lucene.document.Document
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.{Directory, RAMDirectory}
import play.api.{Configuration, Environment}
import play.api.http.Status
import play.api.libs.ws.WSClient
import services.LuceneIndexBuilderActor._
import utils.ClassnameLogger

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}


object LuceneIndexBuilderActor {
  def props: Props = Props[LuceneIndexBuilderActor]

  trait Factory {
    def apply(): Actor
  }

  case class IndexCatalogue(catalogueName: String, catalogueUrl: String)

  case class IndexResponseDocument(cswGetRecordsResponse: CswGetRecordsResponse, catalogueName: String, isLast: Boolean)

  // case class MergeLuceneIndexLocal(directory: Directory, catalogueName: String)

  case class QueryCatalogue(catalogueName: String, catalogueUrl: String)

  case class ShutdownActor(catalogueName: String)

}

/**
  * Builds a Lucene Index from a given CswGetRecordsResponse object
  */
class LuceneIndexBuilderActor @Inject()(configuration: Configuration,
                                        environment: Environment,
                                        wsClient: WSClient,
                                        luceneService: LuceneService)(implicit ec: ExecutionContext) extends Actor with
  ClassnameLogger {

  logger.debug("Reading configuration: csw.maxDocs ")
  private val CSW_MAX_RECORDS = 500
  private val maxDocsPerFetch = configuration.getInt("csw.maxDocs").getOrElse(CSW_MAX_RECORDS)
  private val indexBaseFolder = configuration.getString("searcher.indexCacheDir").getOrElse(".")

  // private val catalogueDirectory = new RAMDirectory()
  private var luceneDocs = scala.collection.mutable.Map[String, Document]()


  /**
    * Defines the initial Actor behaviour. Following behaviours are implemented:
    *   - [[IndexCatalogue]]
    *
    * @return
    */
  override def receive: Receive = {

    case IndexCatalogue(catalogueName, catalogueUrl) => {
      logger.info(s"Initialising to index catalogue $catalogueName")
      context.become(indexing)
      self ! QueryCatalogue(catalogueName, catalogueUrl)
    }
    //FIXME some kind of default behaviour?
  }

  def indexing: Receive = {
    case QueryCatalogue(catalogueName, catalogueUrl) => {
      logger.info(s"Querying $catalogueName on url $catalogueUrl")
      queryCatalogue(catalogueName, catalogueUrl, 1, maxDocsPerFetch)
    }

    case IndexResponseDocument(cswGetRecordsResponse, catalogueName, isLast) => {
      val intermediateMapOfDocuments = extractLuceneDocsFromResponse(cswGetRecordsResponse, catalogueName)
      intermediateMapOfDocuments.foreach (
        tup => luceneDocs += (tup._1 -> tup._2))
      // val directory = buildIndexFor(intermediateMapOfDocuments, catalogueName)
      // sender() ! MergeLuceneIndexLocal(directory, catalogueName)

      // its important to send the shutdown from here, otherwise the last merge will not be executed, because shutdown
      // will be received before this merge message
      if (isLast) {
        logger.info(s"Last response indexed. Sending shutdown for $catalogueName")
        sender() ! ShutdownActor(catalogueName)
      }
    }

//    case MergeLuceneIndexLocal(directory, catalogueName) => {
//      logger.info(s"Merging index inside actor of $catalogueName")
//      val config = new IndexWriterConfig()
//      config.setCommitOnClose(true)
//
//      val iwriter = new IndexWriter(catalogueDirectory, config)
//      try {
//        iwriter.addIndexes(directory)
//        iwriter.commit()
//        logger.info(s"Done merging index for $catalogueName")
//      }
//      finally {
//        iwriter.close()
//      }
//    }

    case ShutdownActor(catalogueName) => {
//      if (catalogueDirectory.listAll().length > 0) {
//        logger.info(s"Merging index for $catalogueName back to main service")
//        luceneService.mergeOverWriteIndex(catalogueDirectory, catalogueName)
//      }
      if (luceneDocs.size > 0) {
        logger.info(s"Merging index (${luceneDocs.size} records) for $catalogueName back to main service")
        luceneService.mergeUpdateDocsIndex(luceneDocs.toMap, catalogueName)
      }

      logger.info(s"Shutting down actor for $catalogueName")
      context.stop(self)
    }
    //FIXME some kind of default behaviour?
  }

  /**
    * Builds a seq of Lucene Documents from a given CswGetRecordsResponse object
    *
    * @param cswGetRecordsResponse
    * @param catalogueName
    * @return
    */
  private def extractLuceneDocsFromResponse(cswGetRecordsResponse: CswGetRecordsResponse, catalogueName: String): Map[String, Document] = {
    logger.info(s"Start building (partial) index for ${catalogueName}...")
    val mdMetadataSet = (cswGetRecordsResponse.xml \\ "MD_Metadata").map(mdMetadataNode => {
      logger.debug(f"Preparing($catalogueName): ${(mdMetadataNode \ "fileIdentifier" \ "CharacterString").text}")
      logger.trace(mdMetadataNode.toString)
      MdMetadataSet.fromXml(mdMetadataNode, catalogueName, luceneService.getCatalogueUrl(catalogueName))
    }).filter(item => item.isDefined).map(item => item.get) //filter out all None values

    mdMetadataSet.map(md => (md.fileIdentifier, md.asLuceneDocument)).toMap
  }

  /**
    * Builds a Lucene Index from a given CswGetRecordsResponse object
    *
    * @param intermediateSetofDocuments Set[Document] to build an index from
    * @param catalogueName         String containing the catalogue name
    * @return [[Directory]] containing the index
    */
  @deprecated
  private def buildIndexFor(intermediateSetofDocuments: Map[String, Document], catalogueName: String): Directory = {

    val directory = new RAMDirectory()
    val config = new IndexWriterConfig()
    config.setCommitOnClose(true)

    //FIXME SR use SCALA_ARM (automated resource management)?
    val iwriter = new IndexWriter(directory, config)
    try {
      iwriter.deleteAll()
      iwriter.commit()
      intermediateSetofDocuments.foreach(tup => iwriter.addDocument(tup._2))
      iwriter.commit()
      logger.info(s"Partial index ready for $catalogueName")
    }
    finally {
      iwriter.close()
    }
    directory
  }

  /**
    * it all happens here
    *
    * @param catalogueName
    * @param catalogueUrl
    * @param startDocument
    * @param documentsToFetch
    */
  private def queryCatalogue(catalogueName: String, catalogueUrl: String, startDocument: Int,
                             documentsToFetch: Int): Unit = {
    val wsClientResponseFuture =
      wsClient.url(catalogueUrl)
//        .withRequestTimeout(20.seconds)
        .withHeaders("Content-Type" -> "application/xml")
        .post(CswGetRecordsRequest(startDocument, documentsToFetch))

    wsClientResponseFuture.onComplete {
      case Success(wsClientResponse) => {
        wsClientResponse.status match {
          case Status.OK => {
            logger.debug(f"Response Content Type: ${wsClientResponse.allHeaders.getOrElse("Content-Type", "Unknown")}")
            logger.debug(f"Response-Length: ${wsClientResponse.body.length}")
            logger.trace(f"Response-Body: ${wsClientResponse.body.toString}")
            wsClientResponse.xml.label match {
              case "ExceptionReport" => {
                logger.warn(
                  f"Got XML Exception Response. Text: ${(wsClientResponse.xml \ "Exception" \ "ExceptionText").text}")
              }
              case "GetRecordsResponse" => {
                val cswGetRecResp = CswGetRecordsResponse(wsClientResponse.xml)
                logger.info(
                  f"c: $catalogueName nextRecord: ${cswGetRecResp.nextRecord}, " +
                    f"numberOfRec ${cswGetRecResp.numberOfRecordsMatched}, " +
                    f"recordsReturned ${cswGetRecResp.numberOfRecordsReturned}")
                if ((cswGetRecResp.nextRecord > cswGetRecResp.numberOfRecordsMatched) ||
                  (cswGetRecResp.nextRecord == 0) ||
                  (cswGetRecResp.numberOfRecordsReturned == 0)) {
                  logger.info(s"Sending IndexResponseDocument - is last! ($catalogueName)")
                  self ! IndexResponseDocument(cswGetRecResp, catalogueName, true)
                }
                else {
                  logger.info(s"Sending IndexResponseDocument - and start another query round. ($catalogueName)")
                  self ! IndexResponseDocument(cswGetRecResp, catalogueName, false)
                  queryCatalogue(catalogueName, catalogueUrl, cswGetRecResp.nextRecord, documentsToFetch)
                }
              }
              case _ => {
                logger.warn(f"Unknown response content. Body: ${wsClientResponse.xml.toString} ($catalogueName)")
                self ! ShutdownActor(catalogueName)
              }
            }
          }
          case _ => {
            self ! ShutdownActor(catalogueName)
          }
        }
      }
      case Failure(ex) => {
        logger.warn(s"Exception while querying CSW $catalogueName (${ex.getClass.getCanonicalName}): ${ex.getMessage}", ex)
        self ! ShutdownActor(catalogueName)
      }
    }
  }
}
