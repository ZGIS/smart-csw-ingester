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
import javax.inject.{Inject, Named, Singleton}

import akka.actor.{ActorRef, ActorSystem, _}
import akka.pattern.ask
import models.gmd.MdMetadataSet
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Document, LongPoint}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search._
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.apache.lucene.spatial.query.{SpatialArgs, SpatialOperation}
import org.apache.lucene.store.{Directory, RAMDirectory}
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io.ShapeIO
import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.concurrent.InjectedActorSupport
import play.api.libs.ws.WSClient
import services.LuceneIndexBuilderActor.IndexCatalogue
import services.LuceneIndexBuilderMasterActor.GetSpecificIndexBuilder
import utils.ClassnameLogger

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

trait IndexService {
  //FIXME SR find a good place for this
  lazy val SPATIAL_CONTEXT = SpatialContext.GEO
  lazy val WORLD_WKT = SPATIAL_CONTEXT.getFormats().getWriter(ShapeIO.WKT).toString(SpatialContext.GEO.getWorldBounds)

  def buildIndex(): Unit

  def query(query: String,
            bboxWkt: Option[String],
            fromDate: Option[LocalDate],
            toDate: Option[LocalDate],
            maxNumberOfResults: Option[Int]
           ): SearchResult
}

/**
  * generic serach result
  */
trait SearchResult {
  /**
    * the parsed query
    */
  val luceneQuery: Query;

  /**
    * number of documents in the index, that match the query. THIS IS NOT THE NUMBER OF RETURNED DOCUMENTS!
    */
  val numberOfMatchingDocuments: Int;

  /**
    * the documents returned by the query. This might be less than the matching number, depending on wether
    * the user choose to get a maximum count.
    */
  val documents: List[MdMetadataSet];
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
                              configuration: Configuration,
                              system: ActorSystem,
                              @Named("indexBuilderMaster") indexBuilderMaster: ActorRef)
  extends IndexService with ClassnameLogger with InjectedActorSupport {

  logger.info("Starting Lucene Service")

  logger.debug("Reading configuration: csw.catalogues ")
  private val cataloguesConfig = configuration.getConfigList("csw.catalogues").get.asScala.toList
  private val catalogues = cataloguesConfig.map({ item => item.getString("name").get -> item.getString("url").get }).toMap
  private val catalogueIndexes: scala.collection.mutable.Map[String, Directory] = scala.collection.mutable.Map[String, Directory]()
  private val defaultMaxDocuments: Int = configuration.getInt("searcher.defaultMaxDocuments").getOrElse(100);

  //stores the search index in RAM
  private val indexRamDirectory = new RAMDirectory()
  private val searcherManager = new SearcherManager(prepareEmptyIndex(indexRamDirectory), null)

  buildIndex()

  appLifecycle.addStopHook { () =>
    Future.successful(() => {
      logger.info("Stopping Lucene Service")
      indexRamDirectory.close()
    })
  }

  def buildIndex(): Unit = {
    catalogues.map({
      case (catalogueName, catalogueUrl) =>
        system.scheduler.schedule(1.second, 24.hours){
          buildIndex(catalogueName)
        }
    })
  }

  /** Builds index for catalogue
    *
    */
  def buildIndex(catalogueName: String): Unit = {
    logger.info(s"Building Lucene Index (AKKA VERSION) for $catalogueName")
    val catalogueUrl = catalogues(catalogueName)
    implicit val timeout: akka.util.Timeout = 5.seconds
    val indexBuilder = (indexBuilderMaster ? GetSpecificIndexBuilder(catalogueName)).mapTo[ActorRef]
    indexBuilder.onComplete({
      case Success(indexBuilder) => indexBuilder ! IndexCatalogue(catalogueName, catalogueUrl)
      case Failure(ex) => logger.warn(s"Exception during index build ${ex.getMessage}", ex)
    })
  }

  def mergeIndex(directory: Directory, catalogueName: String): Unit = {
    catalogueIndexes += (catalogueName -> directory)

    val config = new IndexWriterConfig()
    config.setCommitOnClose(true)

    val iwriter = new IndexWriter(indexRamDirectory, config)
    try {
      logger.info(s"Merging ${catalogueIndexes.keys} into master index")
      iwriter.deleteAll()
      iwriter.commit()
      iwriter.addIndexes(catalogueIndexes.values.toArray : _*)
      iwriter.commit()
      iwriter.forceMerge(1, true)
      logger.info("Merged Index ready")
    }
    finally {
      iwriter.close()
    }
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
            toDate: Option[LocalDate] = None,
            maxNumberOfResults: Option[Int] = None): SearchResult = {

    val textQuery = parseQueryString(query)

    val dateQuery = LongPoint.newRangeQuery("dateStamp",
      fromDate.getOrElse(LocalDate.ofEpochDay(0)).toEpochDay(),
      toDate.getOrElse(LocalDate.now()).toEpochDay())

    val bboxQuery = parseBboxQuery(bboxWtk.filterNot(_.isEmpty).getOrElse(WORLD_WKT))

    searcherManager.maybeRefreshBlocking()
    val isearcher = searcherManager.acquire()

    // TODO TBD AK The way you built it, could omit dates or BBOX query constraint in this
    // this builder if not provided through query params (but particularly date should NOT be limited to 1970 to now?)
    val booleanQueryBuilder = new BooleanQuery.Builder()
    booleanQueryBuilder.add(textQuery, BooleanClause.Occur.MUST)
    booleanQueryBuilder.add(dateQuery, BooleanClause.Occur.MUST)
    booleanQueryBuilder.add(bboxQuery, BooleanClause.Occur.MUST)
    val finalLuceneQuery = booleanQueryBuilder.build()

    val numOfMatchingDocuments = isearcher.count(finalLuceneQuery);

    // TODO SR is "all" a good default, when queried for 0 or less documents?
    val maxDocuments = maxNumberOfResults match {
      case Some(x) if maxNumberOfResults.get <= 0 => numOfMatchingDocuments + 1 //+1 just in case 0 docs match
      case _ => maxNumberOfResults.getOrElse(defaultMaxDocuments)
    }
    val search = isearcher.search(finalLuceneQuery, maxDocuments);
    val scoreDocs = search.scoreDocs

    val results = scoreDocs.map(scoreDoc => {
      val doc = isearcher.doc(scoreDoc.doc)
      MdMetadataSet.fromLuceneDoc(doc)
    }).toList

    //FIXME SR use ARM --> possible mem leak
    searcherManager.release(isearcher)

    new SearchResult {
      override val luceneQuery: Query = finalLuceneQuery
      override val numberOfMatchingDocuments: Int = numOfMatchingDocuments
      override val documents: List[MdMetadataSet] = results
    }
  }

  /**
    * returns the URL of a Catalogue.
    * @param name
    * @return
    */
  def getCatalogueUrl(name: String): String = {
    catalogues(name)
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
    logger.debug(s"create query for $bboxWkt")

    val envelopeWktRegex =
      "ENVELOPE\\(([-+]?[0-9]*\\.?[0-9]+),([-+]?[0-9]*\\.?[0-9]+),([-+]?[0-9]*\\.?[0-9]+),([-+]?[0-9]*\\.?[0-9]+)\\)".r

    val shape = bboxWkt.replaceAll("\\s", "") match {
      case envelopeWktRegex(minX, maxX, minY, maxY) => {
        MdMetadataSet.bboxFromCoords(minX.toFloat, maxX.toFloat, minY.toFloat, maxY.toFloat)
      }
      case _ => {
        logger.warn(s"Could not parse WKT '$bboxWkt'. Using WORLD as query BBOX.");
        MdMetadataSet.bboxFromCoords(-180, 180, -90, 90);
      }
    }

    logger.debug(s"parsed shape is ${shape.toString}")
    val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(SPATIAL_CONTEXT, "bbox")
    bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.IsWithin, shape))
  }

  /**
    * creates an empty index to make searches not crash before real documents are in the index.
    *
    * @param directory
    */
  private def prepareEmptyIndex(directory: Directory): Directory = {
    val analyzer = new StandardAnalyzer()
    val config = new IndexWriterConfig(analyzer)
    config.setCommitOnClose(true)
    //FIXME SR use SCALA_ARM (automated resource management)?
    val iwriter = new IndexWriter(directory, config)
    try {
      iwriter.deleteAll()
      iwriter.addDocument(new Document)
      iwriter.commit()
      logger.info("Empty Index ready")
    }
    finally {
      iwriter.close()
    }
    directory
  }
}
