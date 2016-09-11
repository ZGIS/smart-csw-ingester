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

import java.time.LocalDate
import javax.inject._

import models.gmd.{MdMetadataSet, GeoJSONFeatureCollectionWriter}
import play.api.libs.json.Json
import play.api.mvc._
import services.LuceneService

/**
  * Controller that serves results from Lucene Index
  */
class QueryController @Inject()(luceneService: LuceneService) extends Controller {

  implicit val geoJSONFeatureCollectionWrite = GeoJSONFeatureCollectionWriter
  private lazy val DEFAULT_QUERY = "*:*"

  /**
    * Action that passes the query from URL to the [[services.LuceneService]].
    *
    * @param query
    * @return
    * @see services.LuceneService
    */
  def query(query: Option[String],
            bboxWkt: Option[String],
            fromDateStr: Option[String],
            toDateStr: Option[String]): Action[AnyContent] = Action {

    //TODO SR make tuple expression out of this
    val fromDate = fromDateStr match {
      case None => Some(LocalDate.ofEpochDay(0))
      case _ => Some(MdMetadataSet.dateFromStrings(List(fromDateStr.get)))
    }

    val toDate = toDateStr match {
      case None => Some(LocalDate.now())
      case _ => Some(MdMetadataSet.dateFromStrings(List(toDateStr.get)))
    }

    //FIXME SR error handling
    val featureCollection = luceneService.query(query.getOrElse(DEFAULT_QUERY), bboxWkt, fromDate, toDate)

    // TODO AK here we could insert the query string again if needed
    val resultJson = Json.toJson(featureCollection)

    Ok(resultJson).as(JSON)
  }
}
