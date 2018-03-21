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

import models.ErrorResult
import models.gmd.{GeoJSONFeatureCollectionWriter, MdMetadataSet}
import org.apache.lucene.queryparser.classic.ParseException
import play.api.libs.json._
import play.api.mvc._
import services.LuceneService
import utils.OwcGeoJsonConverters.asOwcResource
import utils.{ClassnameLogger, OwcGeoJsonConverters}

/**
  * Controller that serves results from Lucene Index
  */
// TODO SR rename to "IndexController" or something similar
class QueryController @Inject()(luceneService: LuceneService) extends Controller with ClassnameLogger {

  implicit val geoJSONFeatureCollectionWrite = GeoJSONFeatureCollectionWriter
  private lazy val DEFAULT_QUERY = "*:*"

  /**
    * add lucene score via Json Transform
    *
    * @param json
    * @param searchScore
    * @return
    */
  private def addScoreJsonTransform(json: JsValue, searchScore: Double): JsValue = {

    // FIXME: decide which one
    val jsonTransformer = __.json.update(
      (__ \ 'searchScore).json.put(JsNumber(searchScore)))

    val jsonTransformer2 = JsPath.read[JsObject].map(o => o ++ Json.obj("searchScore" -> JsNumber(searchScore)))
    json.transform(jsonTransformer2).get
  }
  /**
    * converts the featureCollection of the List of [[MdMetadataSet]] to Json,
    * and injects the scores into the [[info.smart.models.owc100.OwcResource]] Json
    *
    * @param featureCollection
    * @param url
    * @return
    */
  private def toOwcContextJsonWithScores(featureCollection: List[MdMetadataSet], url: String): JsValue = {

    val owcResourcesJsList = featureCollection.map{
      md =>
        val owcResource = OwcGeoJsonConverters.asOwcResource(md, url)
        val owcJson = owcResource.toJson
        val owcJsWithScore = addScoreJsonTransform(owcJson, md.searchScore)
        owcJsWithScore
    }

    val owcContext = OwcGeoJsonConverters.toOwcContext(featureCollection, url)
    val emptyContext = owcContext.copy(resource = List.empty)
    val emptyContextJson = emptyContext.toJson

    val jsonTransformer = __.json.update(
      (__ \ 'features).json.put(JsArray(owcResourcesJsList)))

    emptyContextJson.transform(jsonTransformer).get
    // OwcGeoJsonConverters.toOwcContext(featureCollection, url).toJson
  }

  /**
    * Action that passes the query from URL to the [[services.LuceneService]].
    *
    * @param query              Option[String] that contains the Lucene query. Defaults to [[QueryController.DEFAULT_QUERY]]
    * @param bboxWkt            Option[String] that contains the bbox (as WKT) to search in.
    * @param fromDateStr        Option[String] containing an ISO-DATE for lower date bound
    * @param toDateStr          Option[String] containing an ISO-DATE for upper date bound
    * @param maxNumberOfResults Option[Int] containing the max number of documents to return
    * @param contentType        Option[String] containing the type, of the result document. Valid values: "GeoJson" and "OwcContext". Defaults to "GeoJson"
    * @see [[services.LuceneService]]
    * @return
    */
  def query(query: Option[String],
            bboxWkt: Option[String],
            fromDateStr: Option[String],
            toDateStr: Option[String],
            maxNumberOfResults: Option[Int],
            contentType: Option[String]): Action[AnyContent] = Action { implicit request =>
    logger.info(s"Query CSW: ${query.getOrElse("NONE")}")
    // TODO SR make tuple expression out of this
    val fromDate = fromDateStr match {
      case None => Some(LocalDate.ofEpochDay(0))
      case _ => Some(MdMetadataSet.dateFromStrings(List(fromDateStr.get)))
    }

    val toDate = toDateStr match {
      case None => Some(LocalDate.now())
      case _ => Some(MdMetadataSet.dateFromStrings(List(toDateStr.get)))
    }

    try {
      val searchResult = luceneService.query(query.getOrElse(DEFAULT_QUERY), bboxWkt, fromDate, toDate,
        maxNumberOfResults)
      val featureCollection = searchResult.documents

      contentType.getOrElse("GeoJson") match {
        case "GeoJson" => {
          // TODO AK here we could insert the query string again if needed
          val jsonTransformer = __.json.update(
            (__ \ 'countMatched).json.put(JsNumber(searchResult.numberOfMatchingDocuments)))
          val resultJson: JsValue = Json.toJson(featureCollection)

          Ok(resultJson.transform(jsonTransformer).get).as(JSON)
        }
        case "OwcContext" => {
          // FIXME SR we should use the standardized "FORWARDED" header. Or better both? I hate HTTP(S)!
          // See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Forwarded
          val proto = request.headers.get("X-Forwarded-Proto").getOrElse("http")
          val host = request.headers.get("X-Forwarded-Host").getOrElse(request.headers("host"))
          val url = s"${proto}://${request.headers("host")}${request.uri}"
          logger.debug(s"Requested URL by client: ${url}")
          val resultJson = toOwcContextJsonWithScores(featureCollection = featureCollection, url = url)
          Ok(resultJson).as(JSON)
        }
        case _ => BadRequest(Json.toJson(ErrorResult(s"Unknown content type: ${contentType}", None))).as(JSON)
      }
    }
    catch {
      case e: ParseException => {
        logger.error("Exception parsing query: " + e.getMessage, e);
        val error: ErrorResult = ErrorResult(s"Could not parse query '${query.get}' ", Some(e.getMessage))
        InternalServerError(Json.toJson(error)).as(JSON)
      }
    }
  }

  def buildIndexFor(catalogueName: String): Action[AnyContent] = Action {
    logger.info(s"Request for building index for $catalogueName")
    luceneService.buildIndex(catalogueName)
    Ok(s"building index for $catalogueName")
  }

  def deleteFromIndex(fileIdentifier: String): Action[AnyContent] = Action {
    logger.info(s"Request for deleting from default index for id $fileIdentifier")
    if (luceneService.deleteFromIndex(fileIdentifier)) {
      Ok(s"deleted from index $fileIdentifier")
    } else {
      BadRequest("deletion from index caused error.")
    }
  }
}
