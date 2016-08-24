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

import java.time.Month

import models.{GmdElementSet, GmdElementSetJsonWriter}
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io.ShapeIO
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.Json

import scala.xml.Elem

/**
  * Created by steffen on 23.08.16.
  */
class GmdElementSetSpec extends PlaySpec {

  "should build format case classes" in {
    val ctx = SpatialContext.GEO
    val shpReader = ctx.getFormats().getReader(ShapeIO.WKT)
    val shpWriter = ctx.getFormats().getWriter(ShapeIO.GeoJSON)

    val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
    val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)

    val localDate = java.time.LocalDate.of(2012, Month.DECEMBER, 20)

    val gmdElem1 = GmdElementSet.fromXml( xml1)

    val bbox = GmdElementSet.bboxFromXml(xml1)
    val bboxs = ctx.getShapeFactory().rect(-176.176448433, 166.6899599, -47.1549297167, -34.4322590833 )
    bbox mustEqual bboxs

    val gmdElem2 = GmdElementSet("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d",
      localDate,
      "NZ Primary Road Parcels",
      "This layer provides the **current** road parcel polygons with ...",
      List("New Zealand"),
      List("boundaries", "planningCadastre"),
      "omit, Omit",
      "LINZ - Land Information New Zealand, LINZ - Land Information New Zealand, ANZLIC the Spatial Information Council",
      "info@linz.govt.nz, info@linz.govt.nz",
      "Crown copyright reserved, Released under Creative Commons By with: Following Disclaimers..., Crown copyright reserved, Released under Creative Commons By",
      bbox,
      "")

    gmdElem1.fileIdentifier mustEqual gmdElem2.fileIdentifier
    gmdElem1.dateStamp mustEqual gmdElem2.dateStamp
    gmdElem1.title mustEqual gmdElem2.title
    gmdElem1.abstrakt mustEqual gmdElem2.abstrakt
    gmdElem1.keywords mustEqual gmdElem2.keywords
    gmdElem1.topicCategory mustEqual gmdElem2.topicCategory
    gmdElem1.contactName mustEqual gmdElem2.contactName
    gmdElem1.contactOrg mustEqual gmdElem2.contactOrg
    gmdElem1.contactEmail mustEqual gmdElem2.contactEmail
    gmdElem1.license mustEqual gmdElem2.license
    gmdElem1.bbox mustEqual gmdElem2.bbox

    gmdElem1 mustEqual gmdElem2

    gmdElem1.isoLocalDateText() mustEqual("2012-12-20")
    gmdElem1.getUuid() mustEqual(java.util.UUID.fromString("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"))

    val thrown = the [java.lang.IllegalArgumentException] thrownBy java.util.UUID.fromString("urn:uuid:2c5f1309-d721-4299-88bf-e462c577b99a-horowhenua_ws:ewt_nzprj_new")
    thrown.getMessage mustBe ("Invalid UUID string: urn:uuid:2c5f1309-d721-4299-88bf-e462c577b99a-horowhenua_ws:ewt_nzprj_new")

    implicit val gmdElementSetWrite = GmdElementSetJsonWriter

    val gmdList = List(gmdElem1,gmdElem2)
    // println(gmdElem1.toString())
    // println(Json.toJson(gmdElem2))
    // println(Json.toJson(gmdList))
    val jsList = Json.toJson(gmdList)
    (jsList \\ "fileIdentifier").size mustBe 2

    val textJson = """{"fileIdentifier":"23bdd7a3-fd21-daf1-7825-0d3bdc256f9d","dateStamp":"2012-12-20","title":"NZ Primary Road Parcels","abstrakt":"This layer provides the **current** road parcel polygons with ...","keywords":["New Zealand"],"topicCategory":["boundaries","planningCadastre"],"contactName":"omit, Omit","contactOrg":"LINZ - Land Information New Zealand, LINZ - Land Information New Zealand, ANZLIC the Spatial Information Council","contactEmail":"info@linz.govt.nz, info@linz.govt.nz","license":"Crown copyright reserved, Released under Creative Commons By with: Following Disclaimers..., Crown copyright reserved, Released under Creative Commons By","bbox":[-176.176448433,166.6899599,-34.4322590833,-47.1549297167],"origin":""}"""
    Json.toJson(gmdElem2).toString() mustEqual(textJson)

  }

  "should parse COMPLETE GmdElementSet" in {
    val asResource = this.getClass().getResource("MD_Metadata_COMPLETE.xml")
    val xml = scala.xml.XML.load(asResource)
    val parsedElement = GmdElementSet.fromXml(xml)
    parsedElement.fileIdentifier mustEqual("294f127b-addb-24d8-0df1-f014032dcd02")
  }

  "should parse NO_BBOX GmdElementSet" in {
    val asResource = this.getClass().getResource("MD_Metadata_NO_BBOX.xml")
    val xml = scala.xml.XML.load(asResource)
    val parsedElement = GmdElementSet.fromXml(xml)
    parsedElement.fileIdentifier mustEqual("4a5021fa-b85d-cdb7-c6aa-53ef470da713")
  }

  "should parse No dateStamp/Date and find date/CI_Date/date/Date GmdElementSet" in {
    val asResource = this.getClass().getResource("sac_rs_ewt_metadata.xml")
    val xml = scala.xml.XML.load(asResource)
    val parsedElement = GmdElementSet.fromXml(xml)
    parsedElement.isoLocalDateText() mustEqual("2016-05-17")
  }

}
