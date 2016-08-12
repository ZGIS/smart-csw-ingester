
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import play.api.inject.guice.GuiceApplicationBuilder
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Document, Field, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{MatchAllDocsQuery, SearcherManager}
import org.apache.lucene.store.RAMDirectory
import services.{SearchResultDocument, SearchResultHeader}

class LuceneSpec extends PlaySpec with OneAppPerSuite {

  // Override app if you need a Application with other than
  // default parameters.
  // val application: Application = GuiceApplicationBuilder().build()

  implicit override lazy val app = new GuiceApplicationBuilder().configure(Map("ehcacheplugin" -> "disabled")).build()

  "LuceneSpecs with OneAppPerSuite trait " must {

    "provide a Lucene Setup" in {

      val directory = new RAMDirectory()
      val analyzer = new StandardAnalyzer()
      val config = new IndexWriterConfig(analyzer)

      val testList = List(
        SearchResultDocument("Document 1", "Abstract 1", "dummy ctl"),
        SearchResultDocument("Document 2", "Abstract 2", "dummy ctl"),
        SearchResultDocument("Document 3", "Abstract 3", "other ctl"),
        SearchResultDocument("Document 4", "Abstract 4 is longer", "other ctl")
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
      val query2 = "title:\"Document 1\""
      val luceneQuery1 = new MatchAllDocsQuery()

      val luceneQuery2 = parser.parse(query2)

      val search1 = isearcher.search(luceneQuery1, isearcher.getIndexReader().numDocs())
      val scoreDocs1 = search1.scoreDocs

      val search2 = isearcher.search(luceneQuery2, isearcher.getIndexReader().numDocs())
      val scoreDocs2 = search2.scoreDocs

      search1.totalHits mustBe 4
      search2.totalHits mustBe 1

      directory.close()
    }
  }

}