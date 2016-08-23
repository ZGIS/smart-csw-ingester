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

import javax.inject.{Inject, Singleton}

import models.GmdElementSet
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{MatchAllDocsQuery, SearcherManager}
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.apache.lucene.store.RAMDirectory
import org.locationtech.spatial4j.context.SpatialContext
import play.api.inject.ApplicationLifecycle
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WSClient
import utils.ClassnameLogger

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.xml.NodeSeq


// explanation see https://www.playframework.com/documentation/2.5.x/ScalaJsonAutomated and
// https://www.playframework.com/documentation/2.5.x/ScalaJsonCombinators

case class SearchResultHeader(noDocuments: Int, query: String)

case class SearchResult(header: SearchResultHeader, results: List[GmdElementSet])

trait IndexService {
  def refreshIndex(): Unit

  def query(query: String): SearchResult
}

@Singleton
class LuceneService @Inject()(appLifecycle: ApplicationLifecycle, wsClient: WSClient) extends IndexService with
  ClassnameLogger {

  //TODO SR extract to 'RequestBuilder' class. Parameters startPosition="1" maxRecords="15"
  val XML_REQUEST: String =
    """
      |<csw:GetRecords xmlns:csw="http://www.opengis.net/cat/csw/2.0.2"
      |                xmlns:ogc="http://www.opengis.net/ogc"
      |                xmlns:gmd="http://www.isotc211.org/2005/gmd"
      |                service="CSW" version="2.0.2"
      |                resultType="results" startPosition="1" maxRecords="50"
      |                outputFormat="application/xml" outputSchema="http://www.isotc211.org/2005/gmd"
      |                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      |                xsi:schemaLocation="http://www.opengis.net/cat/csw/2.0.2 http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd">
      |  <csw:Query typeNames="gmd:MD_Metadata">
      |    <csw:ElementSetName>full</csw:ElementSetName>
      |  </csw:Query>
      |</csw:GetRecords>
    """.stripMargin

  val EMPTY_RESPONSE: NodeSeq = <csw:GetRecordsResponse xsi:schemaLocation="http://www.opengis.net/cat/csw/2.0.2 http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd"
                                                        version="2.0.2" xmlns:sitemap="http://www.sitemaps.org/schemas/sitemap/0.9"
                                                        xmlns:csw="http://www.opengis.net/cat/csw/2.0.2">
    <csw:SearchStatus timestamp="2016-08-24T02:01:33Z"/>
    <csw:SearchResults elementSet="full" recordSchema="http://www.isotc211.org/2005/gmd"
                       numberOfRecordsReturned="0" numberOfRecordsMatched="0" nextRecord="0">
    </csw:SearchResults>
  </csw:GetRecordsResponse>

  logger.info("Starting Lucene Service")

  //stores the search index in RAM
  val directory = new RAMDirectory()
  refreshIndex()
  //as long as there is no index, we cant have a searcher manager. Figure out how to do :-)

  val searcherManager = new SearcherManager(directory, null)

  appLifecycle.addStopHook { () =>
    Future.successful(() => {
      logger.info("Stopping Lucene Service")
      directory.close()
    })
  }

  /**
    * Refreshes the Search index
    */
  //FIXME SR use something like lib-lucene-sugar. Lib unfortunately seems very old/outdated? https://github.com/gilt/lib-lucene-sugar
  def refreshIndex(): Unit = {
    logger.info("Refreshing Lucene Index")
    val analyzer = new StandardAnalyzer()
    val config = new IndexWriterConfig(analyzer)
    config.setCommitOnClose(true)

    //FIXME SR use SCALA_ARM (automated resource management)?
    val iwriter = new IndexWriter(directory, config)
    try {
      iwriter.deleteAll()
      iwriter.commit()

      val url = "http://data.linz.govt.nz/feeds/csw/csw"
      val result = Await.result(queryCsw(url, "linz"), 120.seconds)
      logger.info(f"Loaded ${result.size} documents from CSW.")
      result.foreach(searchDocument => iwriter.addDocument(searchDocument.asLuceneDocument()))
      iwriter.commit()
      logger.info("Index ready")
    }
    finally {
      iwriter.close()
    }
  }

  def queryCsw(url: String, catalogueId: String): Future[List[GmdElementSet]] = {
    logger.info(f"Loading data from CSW $url")

    val wsClientRequestFuture = wsClient.url(url)
      .withRequestTimeout(60.seconds)
      .withHeaders("Content-Type" -> "application/xml")
      .post(XML_REQUEST)

    val requestResult = wsClientRequestFuture.map {
      response => logger.debug(f"Response status: ${response.status} - ${response.statusText}")
        response.status match {
          case 200 => {
            logger.debug(f"Response Content Type: ${response.allHeaders.get("Content-Type").getOrElse("Unknown")}")
            logger.debug(f"Response-Length: ${response.body.length}")
            logger.trace(f"Response-Body: ${response.body.toString}")
            response.xml.label match {
              case "ExceptionReport" => {
                logger.warn(f"Got Exception Response. Text: ${(response.xml \ "Exception" \ "ExceptionText").text}")
                None
              }
              case "GetRecordsResponse" => {
                Some(response.xml)
              }
              case _ => {
                logger.warn(f"Unknown response content. Body: ${response.xml.toString}")
                None
              }
            }
          }
          case _ => {
            logger.warn(f"Error while executing CSW query.")
            logger.debug(f"Response body: ${response.body}")
            None
          }
        }
    }

    val gmdElementSets = requestResult.map(xmlOption => {
      val resultElements = (xmlOption.getOrElse(EMPTY_RESPONSE) \\ "MD_Metadata").map {
        node => {
          logger.debug(f"Preparing ${(node \\ "fileIdentifier" \ "CharacterString").text}")
          logger.trace(node.toString())
          GmdElementSet.fromXml(node, catalogueId)
        }
      }.toList
      resultElements
    })
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
        //FIXME errorhandling
        parser.parse(query)
      }
    }

    searcherManager.maybeRefreshBlocking()
    val isearcher = searcherManager.acquire()

    //FIXME SR when index empty, the next
    val search = isearcher.search(luceneQuery, isearcher.getIndexReader().numDocs())
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
