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