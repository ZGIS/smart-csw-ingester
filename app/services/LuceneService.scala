package services

import javax.inject.{Inject, Singleton}

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Document, Field, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{MatchAllDocsQuery, SearcherManager}
import org.apache.lucene.store.RAMDirectory

import play.api.Logger
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient

import scala.concurrent.Future

// explanation see https://www.playframework.com/documentation/2.5.x/ScalaJsonAutomated and
// https://www.playframework.com/documentation/2.5.x/ScalaJsonCombinators
case class SearchResultDocument(title: String, abstrakt: String, catalogue: String) {
  def asLuceneDocument(): Document = {
    val doc = new Document()
    doc.add(new Field("title", title, TextField.TYPE_STORED))
    doc.add(new Field("abstract", abstrakt, TextField.TYPE_STORED))
    doc.add(new Field("catalogue", catalogue, TextField.TYPE_STORED))
    //FIXME decide if use catch_all field or how to build a query that queries all fields
    doc.add(new Field("catch_all", title, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", abstrakt, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", catalogue, TextField.TYPE_STORED))

    doc
  }
}

case class SearchResultHeader(noDocuments: Int, query: String)

case class SearchResult(header: SearchResultHeader, results: List[SearchResultDocument])

/**
  * Wraps around Lucene.
  */
@Singleton
class LuceneService @Inject()(appLifecycle: ApplicationLifecycle, ws: WSClient) {

  // TODO decide if we want CSW or MD_Metadata as output schema
  // AK gmd:MD_Metadata preferred from my side
  val XML_REQUEST: String =
    """
      |<?xml version="1.0" encoding="UTF-8"?>
      |<csw:GetRecords
      |  xmlns:csw="http://www.opengis.net/cat/csw/2.0.2"
      |  xmlns:ogc="http://www.opengis.net/ogc"
      |  service="CSW" version="2.0.2"
      |  resultType="results" startPosition="1" maxRecords="15"
      |  outputFormat="application/xml" outputSchema="http://www.opengis.net/cat/csw/2.0.2"
      |  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      |  xsi:schemaLocation="http://www.opengis.net/cat/csw/2.0.2 http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd">
      |
      |  <csw:Query typeNames="csw:Record">
      |    <csw:ElementSetName>full</csw:ElementSetName>
      |  </csw:Query>
      |</csw:GetRecords>
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
      queryCsw().foreach(searchDocument => iwriter.addDocument(searchDocument.asLuceneDocument()))
      iwriter.commit()
    }
    finally {
      iwriter.close()
    }
  }

  def queryCsw(): List[SearchResultDocument] = {

    /*
        val future: Future[scala.xml.NodeSeq] = ws.url("http://data.linz.govt.nz/feeds/csw/csw")
          .withHeaders("Content-Type" -> "application/xml")
          .post(XML_REQUEST).map({response => response.xml})
    */
    List(
      SearchResultDocument("Document 1", "Abstract 1", "dummy ctl"),
      SearchResultDocument("Document 2", "Abstract 2", "dummy ctl"),
      SearchResultDocument("Document 3", "Abstract 3", "other ctl"),
      SearchResultDocument("Document 4", "Abstract 4 is longer", "other ctl")
    )
  }

  /**
    * Queries the Search Index
    *
    * @param query
    * @return
    */
  def query(query: String): SearchResult = {

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
      SearchResultDocument(title = doc.get("title"), abstrakt = doc.get("abstract"), catalogue = doc.get("catalogue"))
    })
    SearchResult(header, results.toList)
  }
}
