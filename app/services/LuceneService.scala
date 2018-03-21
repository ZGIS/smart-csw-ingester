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

import java.io.{File, IOException}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDate

import javax.inject.{Inject, Named, Singleton}
import akka.actor.{ActorRef, ActorSystem, _}
import akka.pattern.ask
import models.gmd.MdMetadataSet
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document._
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search._
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.apache.lucene.spatial.query.{SpatialArgs, SpatialOperation}
import org.apache.lucene.store.{Directory, FSDirectory, RAMDirectory}
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io.ShapeIO
import play.api.{Configuration, Environment, Mode}
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
import com.sksamuel.avro4s.{AvroInputStream, AvroOutputStream, AvroSchema}
import org.apache.avro.Schema

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
  val luceneQuery: Query

  /**
    * number of documents in the index, that match the query. THIS IS NOT THE NUMBER OF RETURNED DOCUMENTS!
    */
  val numberOfMatchingDocuments: Int

  /**
    * the documents returned by the query. This might be less than the matching number, depending on wether
    * the user choose to get a maximum count.
    */
  val documents: List[MdMetadataSet]
}

case class CatalogueIdState(catalogueName: String, fileIdentifiers: Seq[String])

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
                              environment: Environment,
                              system: ActorSystem,
                              @Named("indexBuilderMaster") indexBuilderMaster: ActorRef)
  extends IndexService with ClassnameLogger with InjectedActorSupport {

  logger.info("Starting Lucene Service")

  logger.debug("Reading configuration: csw.catalogues ")
  private val cataloguesConfig = configuration.getConfigList("csw.catalogues").get.asScala.toList
  private val catalogues = cataloguesConfig.map({ item => item.getString("name").get -> item.getString("url").get }).toMap
  private val catalogueIndexes: scala.collection.mutable.Map[String, Directory] = scala.collection.mutable.Map[String, Directory]()
  private val defaultMaxDocuments: Int = configuration.getInt("searcher.defaultMaxDocuments").getOrElse(100)
  private val indexBaseFolder = configuration.getString("searcher.indexCacheDir").getOrElse(Files.createTempDirectory("lucene-tmp").toAbsolutePath.toString)

  // stores the search index in RAM
  // private val indexRamDirectory = prepareEmptyRamIndex(new RAMDirectory())

  // in fs folder for possible re-open
  private val indexFileSystemDirectory = openOrCreateFileSystemIndex(indexBaseFolder)

  private val catalogueStateDirectory = openOrCreateCatalogueStateDir(indexBaseFolder)
  val avroSchema: Schema = AvroSchema[CatalogueIdState]

  private val indexDirectory = indexFileSystemDirectory

  private val searcherManager = new SearcherManager(indexDirectory, null)

  buildIndex()

  appLifecycle.addStopHook { () =>
    Future.successful(() => {
      logger.info("Stopping Lucene Service")
      indexDirectory.close()
    })
  }

  def buildIndex(): Unit = {
    catalogues.map({
      case (catalogueName, catalogueUrl) =>
        val secDelay = if (environment.mode.equals(Mode.Prod)) 5 + scala.util.Random.nextInt(56) else 1
        val hoursRepeat = 24 + scala.util.Random.nextInt(49)
        logger.info(s"Schedule for next build index of $catalogueName is secDelay: $secDelay and hoursRepeat: $hoursRepeat")
        system.scheduler.schedule(secDelay.second, hoursRepeat.hours) {
          buildIndex(catalogueName)
        }
    })
  }

  // buildOrUpdateIndex (GetCapa updateSequence = "1504693010")

  /** Builds index for catalogue
    *
    */
  def buildIndex(catalogueName: String): Unit = {
    logger.info(s"Building Lucene Index (AKKA VERSION) for $catalogueName")
    val catalogueUrl = catalogues(catalogueName)
    implicit val timeout: akka.util.Timeout = 30.seconds
    val indexBuilder = (indexBuilderMaster ? GetSpecificIndexBuilder(catalogueName)).mapTo[ActorRef]
    indexBuilder.onComplete({
      case Success(indexBuilder) => indexBuilder ! IndexCatalogue(catalogueName, catalogueUrl)
      case Failure(ex) => logger.warn(s"Exception during index build for $catalogueName ${ex.getMessage}", ex)
    })
  }

  /**
    *
    * @param directory
    * @param catalogueName
    */
  @deprecated
  def mergeOverWriteIndex(directory: Directory, catalogueName: String): Unit = {
    catalogueIndexes += (catalogueName -> directory)

    val config = new IndexWriterConfig()
    config.setCommitOnClose(true)

    val iwriter = new IndexWriter(indexDirectory, config)
    try {
      logger.info(s"Merging ${catalogueIndexes.keys} into master index")
      iwriter.deleteAll()
      iwriter.commit()
      iwriter.addIndexes(catalogueIndexes.values.toArray: _*)

      iwriter.commit()
      iwriter.forceMerge(1, true)
      logger.info(s"Overwrite-Merging index $catalogueName into main, ready ")
    }
    finally {
      iwriter.close()
    }
  }

  def mergeUpdateDocsIndex(docs: Map[String, Document], catalogueName: String): Unit = {

    // try load persisted catalogue state id list
    val filename = s"$catalogueName.avro"
    val avroFile = new File(catalogueStateDirectory + File.separator + filename)
    val keys = docs.keys.toSet

    val avroIn = AvroInputStream.data[CatalogueIdState](avroFile)
    val oldCatalogueState = avroIn.iterator.toSeq.headOption
    avroIn.close()

    val inOldButnotInNew: Option[Set[String]] = oldCatalogueState.map { cs =>
      cs.fileIdentifiers.toSet.diff(keys)
    }
    // if existent calcluate the diff of file ids in the old loaded list vs the file ids in the current merge opearation

    // if ids in old, but not in new, go through the lucene directory and try to delete these by id from th ecurrent index

    val config = new IndexWriterConfig()
    config.setCommitOnClose(true)
    config.setOpenMode(OpenMode.CREATE_OR_APPEND)
    val iwriter = new IndexWriter(indexDirectory, config)
    try {
      docs.foreach(
        // don't use, only for index uniqueness and doc update
        tup => iwriter.updateDocument(new Term("id", tup._1), tup._2))

      iwriter.commit()
      // iwriter.forceMerge(1, true)
      logger.info(s"Update-merging index $catalogueName into main, ready ")

      inOldButnotInNew.map(diffSet =>
        diffSet.map(id => try {
          iwriter.deleteDocuments(new Term("id", id))
        }))

      iwriter.commit()
      // iwriter.forceMerge(1, true)
      logger.info(s"Update-merging remove old ids from $catalogueName into main, ready ")

    }
    finally {
      iwriter.close()
    }

    // persist the new state for the catalogue
    val avroOut = AvroOutputStream.data[CatalogueIdState](avroFile)
    try {
      avroOut.write(CatalogueIdState(catalogueName, keys.toSeq))
      avroOut.flush()
      logger.info(s"Updating catalogue state persistence for $catalogueName, ready ")
    } finally {
      avroOut.close()
    }
  }

  /**
    * try t odelete a document
    *
    * @param fileIdentifier
    * @return
    */
  def deleteFromIndex(fileIdentifier: String): Boolean = {
    val config = new IndexWriterConfig()
    config.setCommitOnClose(true)
    val iwriter = new IndexWriter(indexDirectory, config)
    try {
      // don't use, only for index uniqueness and doc update
      iwriter.deleteDocuments(new Term("id", fileIdentifier))

      iwriter.commit()
      // iwriter.forceMerge(1, true)
      logger.info(s"deleting $fileIdentifier from index, ok ")
      true
    } catch {
      case ex: IOException => {
        logger.error(s"deleting $fileIdentifier from index, IO error: " + ex.getLocalizedMessage)
        false
      }
      case _ => {
        logger.error(s"deleting $fileIdentifier from index, other unspecified error: ")
        false
      }
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

    val numOfMatchingDocuments = isearcher.count(finalLuceneQuery)

    // TODO SR is "all" a good default, when queried for 0 or less documents?
    val maxDocuments = maxNumberOfResults match {
      case Some(x) if maxNumberOfResults.get <= 0 => numOfMatchingDocuments + 1 //+1 just in case 0 docs match
      case _ => maxNumberOfResults.getOrElse(defaultMaxDocuments)
    }
    val search = isearcher.search(finalLuceneQuery, maxDocuments)
    val scoreDocs = search.scoreDocs

    val results = scoreDocs.map(scoreDoc => {
      val doc = isearcher.doc(scoreDoc.doc)
      MdMetadataSet.fromLuceneDoc(doc, scoreDoc.score)
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
    *
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
    // bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.IsWithin, shape))
    // FIXME AK intersects vs iswithin, should be selectable
    bboxStrategy.makeQuery(new SpatialArgs(SpatialOperation.Intersects, shape))
  }

  /**
    * creates an empty index to make searches not crash before real documents are in the index.
    *
    * @param directory
    */
  @deprecated
  private def prepareEmptyRamIndex(directory: Directory): Directory = {
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

  /**
    * create a filesystem based lucene directory
    *
    * @param indexBaseFolder
    * @return
    */
  private def openOrCreateFileSystemIndex(indexBaseFolder: String): Directory = {

    val baseIndexDir = Paths.get(indexBaseFolder)
    if (!Files.isWritable(baseIndexDir)) {
      val errmsg = s"Main index storage directory ${baseIndexDir.toAbsolutePath} does not exist or is not writable, please check the path"
      logger.error(errmsg)
      throw new IOException(errmsg)
    }
    val jointIndexDir = new File(Paths.get(indexBaseFolder) + File.separator + "main")

    val preExistent = if (Files.isWritable(jointIndexDir.toPath)) {
      true
    } else {
      if (jointIndexDir.mkdir()) {
        false
      } else {
        val errmsg = s"Joint index sub-directory ${jointIndexDir.toPath.toString} could not be created"
        logger.error(errmsg)
        throw new IOException(errmsg)
      }
    }

    val directory = FSDirectory.open(jointIndexDir.toPath)
    val analyzer = new StandardAnalyzer()
    val config = new IndexWriterConfig(analyzer)
    config.setCommitOnClose(true)
    if (preExistent) {
      // Add new documents to an existing index:
      config.setOpenMode(OpenMode.CREATE_OR_APPEND)
      val iwriter = new IndexWriter(directory, config)

      try {
        val emptyDoc = new Document()
        // id field not used in our search semantics, only to provide lucene uniqueness
        emptyDoc.add(new StringField("id", "00000000-0000-0000-0000-000000000000", Field.Store.NO))
        iwriter.updateDocument(new Term("fileIdentifier", "00000000-0000-0000-0000-000000000000"), emptyDoc)
        iwriter.commit()
        logger.info("Re-opened main index ready")
      }
      finally {
        iwriter.close()
      }
    } else {
      // Create a new index in the directory, removing any
      // previously indexed documents:
      config.setOpenMode(OpenMode.CREATE)
      val iwriter = new IndexWriter(directory, config)

      try {
        iwriter.deleteAll()
        iwriter.addDocument(new Document)
        iwriter.commit()
        logger.info("Empty main index ready")
      }
      finally {
        iwriter.close()
      }
    }

    directory
  }

  /**
    * creates the folder for the avro file identifier persistence list
    *
    * @param indexBaseFolder
    * @return
    */
  private def openOrCreateCatalogueStateDir(indexBaseFolder: String): Path = {
    val baseIndexDir = Paths.get(indexBaseFolder)
    if (!Files.isWritable(baseIndexDir)) {
      val errmsg = s"Main index storage directory ${baseIndexDir.toAbsolutePath} does not exist or is not writable, please check the path"
      logger.error(errmsg)
      throw new IOException(errmsg)
    }
    val catalogueStateDir = new File(Paths.get(indexBaseFolder) + File.separator + "state")

    val preExistent = if (Files.isWritable(catalogueStateDir.toPath)) {
      true
    } else {
      if (catalogueStateDir.mkdir()) {
        false
      } else {
        val errmsg = s"catalogue state sub-directory ${catalogueStateDir.toPath.toString} could not be created"
        logger.error(errmsg)
        throw new IOException(errmsg)
      }
    }

    catalogueStateDir.toPath
  }
}
