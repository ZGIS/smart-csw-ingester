package controllers

import javax.inject._

import play.api.libs.json.{JsString, JsValue, Json}
import play.api.mvc._
import services.{LuceneService, SearchResultDocument, SearchResultHeader, SearchResult}


/**
  * Controller that serves results from Lucene Index
  */
class QueryController @Inject()(luceneService: LuceneService) extends Controller {

  // explanation see https://www.playframework.com/documentation/2.5.x/ScalaJsonAutomated
  implicit val searchResultHeaderRead = Json.reads[SearchResultHeader]
  implicit val searchResultHeaderWrite = Json.writes[SearchResultHeader]
  implicit val searchResultDocumentRead = Json.reads[SearchResultDocument]
  implicit val searchResultDocumentWrite = Json.writes[SearchResultDocument]
  implicit val searchResultRead = Json.reads[SearchResult]
  implicit val searchResultWrite = Json.writes[SearchResult]


  /** passes query on to Lucene search and returns result */
  def query(query: String) = Action {
    //TODO this must be something like val result = Lucene.Search(query)
    val searchResult = luceneService.query(query)

    Ok(Json.toJson(searchResult)).as(JSON)
  }
}