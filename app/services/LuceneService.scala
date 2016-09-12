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

import java.time.LocalDate
import javax.inject.{Inject, Singleton}

import models.csw.{CswGetRecordsRequest, CswGetRecordsResponse}
import models.gmd.MdMetadataSet
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.LongPoint
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search._
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.apache.lucene.spatial.query.{SpatialArgs, SpatialOperation}
import org.apache.lucene.store.RAMDirectory
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io.ShapeIO
import play.api.Configuration
import play.api.http.Status
import play.api.inject.ApplicationLifecycle
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WSClient
import utils.ClassnameLogger

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait IndexService {
  //FIXME SR find a good place for this
  //THE ARGUMENT ORDER IS ENVELOPE(minX, maxX, maxY(!!!WTF?), minY)
  lazy val WORLD_WKT = "ENVELOPE(-180, 180, 90, -90)"

  def buildIndex(): Unit

  def query(query: String,
            bboxWkt: Option[String],
            fromDate: Option[LocalDate],
            toDate: Option[LocalDate]): List[MdMetadataSet]
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

  logger.debug("Reading configuration: csw.catalogues ")
  private val cataloguesConfig = configuration.getConfigList("csw.catalogues").get.asScala.toList
  val catalogues = cataloguesConfig.map { item => item.getString("name").get -> item.getString("url").get }.toMap

  logger.debug("Reading configuration: csw.maxDocs ")
  val CSW_MAX_RECORDS = 500
  val maxDocsPerFetch = configuration.getInt("csw.maxDocs").getOrElse(CSW_MAX_RECORDS)

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

    //FIXME SR 5 minutes seem quite arbitraty... what is a good timeout here?
    val gmdElemSets = Await.result(gmdElemSetsFutures, 5.minutes).flatten
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
        //        .withRequestTimeout(20.seconds)
        .withHeaders("Content-Type" -> "application/xml")
        .post(CswGetRecordsRequest(startPosition, maxDocuments))

    val cswGetRecordsResponseListFuture = wsClientResponseFuture.flatMap { wsClientResponse =>
      logger.info(f"Response status: ${wsClientResponse.status} - ${wsClientResponse.statusText} ($url)")
      wsClientResponse.status match {
        case Status.OK => {
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
              logger.info(
                f"nextRecord: ${cswGetRecResp.nextRecord}, numberOfRec ${cswGetRecResp.numberOfRecordsMatched}")
              if ((cswGetRecResp.nextRecord > cswGetRecResp.numberOfRecordsMatched) ||
                (cswGetRecResp.nextRecord == 0)) {
                f.flatMap(l => {
                  Future.successful(cswGetRecResp :: l)
                })
              }
              else {
                f.flatMap(l => {
                  postGetRecordsRequest(url, cswGetRecResp.nextRecord, maxDocuments,
                    Future.successful(cswGetRecResp :: l))
                })
              }
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
  private def queryCatalogue(catalogueName: String, url: String): Future[List[MdMetadataSet]] = {
    val getRecordsFuture = postGetRecordsRequest(url, 1, maxDocsPerFetch, Future.successful(Nil))
    val gmdElementsFuture = getRecordsFuture.map(cswGetRecordsResponses => {
      cswGetRecordsResponses.flatMap(cswGetRecordsResponse => {
        (cswGetRecordsResponse.xml \\ "MD_Metadata").map(mdMetadataNode => {
          logger.debug(f"Preparing($catalogueName): ${(mdMetadataNode \ "fileIdentifier" \ "CharacterString").text}")
          logger.trace(mdMetadataNode.toString)
          MdMetadataSet.fromXml(mdMetadataNode, catalogueName)
        })
      }).filter(item => item.isDefined).map(item => item.get) //filter out all None values
    })
    gmdElementsFuture
  }

  /**
    * parses the query text
    *
    * @param queryString
    * @return
    */
  private def parseQueryString(queryString: String) = {
    queryString.trim() match {
      case "" => new MatchAllDocsQuery()
      case _ => {
        val parser = new QueryParser("catch_all", new StandardAnalyzer())
        //FIXME errorhandling
        parser.parse(queryString)
      }
    }
  }

  /**
    * creates a query, that should find anything inside a BBOX
    *
    * @param bboxWkt
    * @return
    */
  private def parseBboxQuery(bboxWkt: String) = {
    val ctx = SpatialContext.GEO
    val shpReader = ctx.getFormats().getReader(ShapeIO.WKT)
    val shape = shpReader.read(bboxWkt)

    val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(ctx, "bbox")
    bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.IsWithin, shape))
  }

  /**
    * Queries the Search Index
    *
    * @param query
    * @return
    */
  def query(query: String,
            bboxWtk: Option[String] = None,
            fromDate: Option[LocalDate] = None,
            toDate: Option[LocalDate] = None): List[MdMetadataSet] = {

    val textQuery = parseQueryString(query)

    val dateQuery = LongPoint.newRangeQuery("dateStamp",
      fromDate.getOrElse(LocalDate.ofEpochDay(0)).toEpochDay(),
      toDate.getOrElse(LocalDate.now()).toEpochDay())

    val bboxQuery = parseBboxQuery(bboxWtk.filterNot(_.isEmpty).getOrElse(WORLD_WKT))

    //FIXME SR when index empty, the next call will fail
    val searcherManager = new SearcherManager(directory, null)
    searcherManager.maybeRefreshBlocking()
    val isearcher = searcherManager.acquire()

    // TODO TBD AK The way you built it, could omit dates or BBOX query constraint in this
    // this builder if not provided through query params (but particularly date should NOT be limited to 1970 to now?)
    val booleanQueryBuilder = new BooleanQuery.Builder()
    booleanQueryBuilder.add(textQuery, BooleanClause.Occur.MUST)
    booleanQueryBuilder.add(dateQuery, BooleanClause.Occur.MUST)
    booleanQueryBuilder.add(bboxQuery, BooleanClause.Occur.MUST)
    val luceneQuery = booleanQueryBuilder.build()

    val search = isearcher.search(luceneQuery, isearcher.getIndexReader.numDocs())
    val scoreDocs = search.scoreDocs

    val results = scoreDocs.map(scoreDoc => {
      val doc = isearcher.doc(scoreDoc.doc)
      MdMetadataSet.fromLuceneDoc(doc)
    }).toList

    //FIXME SR use ARM --> possible mem leak
    searcherManager.release(isearcher)

    results

  }
}
