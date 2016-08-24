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

import java.time._
import java.time.format._

import models.{GmdElementSet, GmdElementSetJsonWriter}
import org.scalatestplus.play._
import play.api.inject.guice.GuiceApplicationBuilder

import scala.xml._
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io._
import org.locationtech.spatial4j.shape._
import org.locationtech.spatial4j.shape.impl.BBoxCalculator
import play.api.libs.json.Json

class ParserSpec extends PlaySpec {

  "ParserSpecs basics " must {

    "Load XML Test Resource" in {
      val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
      val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)
      xml1.toString() must include ("Land Information New Zealand")

      val asResource2 = this.getClass().getResource("sac_rs_ewt_metadata.xml")
      val xml2: scala.xml.NodeSeq = scala.xml.XML.load(asResource2)
      xml2.text.contains("Hydrogeology") mustEqual(true)

    }

    /*
    Scala XML \, \\, and @ operators:
    - \ and \\ are "projection functions", and return a NodeSeq object
    - \ and \\ operators (functions) are based on XPath operators, but Scala uses backslashes instead of forward-slashes because forward-slashes are already used for math operations
    - The \ operator doesn't descend into child elements
    - "@" character to search for XML tag attributes
    - nameSpace handling can be omitted if obvious / unambiguous element name
    - http://www.codecommit.com/blog/scala/working-with-scalas-xml-support
     */
    "Evaluate Basic Xpath type queries" in {
      val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
      val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)

      val pp = new scala.xml.PrettyPrinter(80,5)
      val nsGmd = "http://www.isotc211.org/2005/gmd"
      val nsGco = "http://www.isotc211.org/2005/gco"
      val nsBindingGmd = new NamespaceBinding("gmd", nsGmd, null)
      val nsBindingGco = new NamespaceBinding("gco", nsGco, nsBindingGmd)

      (xml1 \ "fileIdentifier" \ "CharacterString").text mustBe "23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"

      val n1: NodeSeq = (xml1 \ "fileIdentifier" \ "CharacterString")
      n1.filter ( x => x.namespace == nsGmd ).isEmpty mustBe(true)

      val n2: NodeSeq = (xml1 \ "fileIdentifier" \ "CharacterString")
      n2.filter ( x => x.namespace == nsGco ).size mustEqual 1

      (xml1 \\ "identificationInfo" \ "MD_DataIdentification" \ "citation" \ "CI_Citation" \ "title" \ "CharacterString" ).text mustBe "NZ Primary Road Parcels"

      // gmd:hierarchyLevel/gmd:MD_ScopeCode
      (xml1 \\ "hierarchyLevel" \ "MD_ScopeCode" ).text mustBe "dataset"

      //gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:onlineResource/gmd:CI_OnlineResource/gmd:linkage/gmd:URL");
      (xml1 \\ "CI_OnlineResource" \ "linkage" \ "URL" ).text mustBe "https://data.linz.govt.nz/layer/796-nz-primary-road-parcels/"

      //gmd:contact/gmd:CI_ResponsibleParty/gmd:role/gmd:CI_RoleCode/codeList="./resources/codeList.xml#CI_RoleCode" codeListValue="pointOfContact"
      (xml1 \\ "contact" \ "CI_ResponsibleParty" \ "role" \ "CI_RoleCode" ).text mustBe "resourceProvider"

      // gmd:dateStamp/gco:DateTime"
      // (xml1 \\ "dateStamp" \ "DateTime")
      (xml1 \\ "dateStamp" \ "Date").text mustBe "2012-12-20"

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:status/gmd:MD_ProgressCode/codeList="./resources/codeList.xml#MD_ProgressCode" codeListValue="completed">completed
      println( pp.formatNodes(xml1 \\ "identificationInfo" \ "MD_DataIdentification" \ "status" \ "MD_ProgressCode" ))
      (xml1 \\ "identificationInfo" \ "MD_DataIdentification" \ "status" \ "MD_ProgressCode" ).text mustBe "onGoing"

      (xml1 \\ "identificationInfo" \ "MD_DataIdentification" \ "status" \ "MD_ProgressCode" \ "@codeList" ).text mustBe "http://asdd.ga.gov.au/asdd/profileinfo/gmxCodelists.xml#MD_ProgressCode"
      val at1 = (xml1 \\ "identificationInfo" \ "MD_DataIdentification" \ "status" \ "MD_ProgressCode").filter(node => node.attribute("codeList")
        .exists(codeList => codeList.text == "http://asdd.ga.gov.au/asdd/profileinfo/gmxCodelists.xml#MD_ProgressCode"))

      at1.size mustEqual 1
    }

    "should compute bounding boxes" in {
      import java.util.{ArrayList, List}

      val bboxXml = <gmd:extent><gmd:EX_Extent><gmd:geographicElement><gmd:EX_GeographicBoundingBox><gmd:westBoundLongitude><gco:Decimal>166.6899599</gco:Decimal></gmd:westBoundLongitude><gmd:eastBoundLongitude><gco:Decimal>-176.176448433</gco:Decimal></gmd:eastBoundLongitude><gmd:southBoundLatitude><gco:Decimal>-47.1549297167</gco:Decimal></gmd:southBoundLatitude><gmd:northBoundLatitude><gco:Decimal>-34.4322590833</gco:Decimal></gmd:northBoundLatitude></gmd:EX_GeographicBoundingBox></gmd:geographicElement></gmd:EX_Extent></gmd:extent>

      val west = (bboxXml \\ "extent" \ "EX_Extent" \ "geographicElement" \ "EX_GeographicBoundingBox" \ "westBoundLongitude" \ "Decimal").text.toDouble
      val east = (bboxXml \\ "extent" \ "EX_Extent" \ "geographicElement" \ "EX_GeographicBoundingBox" \ "eastBoundLongitude" \ "Decimal").text.toDouble
      val north = (bboxXml \\ "extent" \ "EX_Extent" \ "geographicElement" \ "EX_GeographicBoundingBox" \ "northBoundLatitude" \ "Decimal").text.toDouble
      val south = (bboxXml \\ "extent" \ "EX_Extent" \ "geographicElement" \ "EX_GeographicBoundingBox" \ "southBoundLatitude" \ "Decimal").text.toDouble

      val ctx = SpatialContext.GEO
      val shpReader = ctx.getFormats().getReader(ShapeIO.WKT)
      val shpWriter = ctx.getFormats().getWriter(ShapeIO.GeoJSON)

      val nwWkt = f"POINT($west $north)"
      val neWkt = f"POINT($east $north)"
      val seWkt = f"POINT($east $south)"
      val swWkt = f"POINT($west $south)"

      val p1 = shpReader.read(nwWkt).asInstanceOf[Point]
      val p2 = shpReader.read(neWkt).asInstanceOf[Point]
      val p3 = shpReader.read(seWkt).asInstanceOf[Point]
      val p4 = shpReader.read(swWkt).asInstanceOf[Point]

      val p1s = ctx.getShapeFactory().pointXY(west, north)
      val p2s = ctx.getShapeFactory().pointXY(east, north)
      val p3s = ctx.getShapeFactory().pointXY(east, south)
      val p4s = ctx.getShapeFactory().pointXY(west, south)

      p1 mustEqual p1s
      p2 mustEqual p2s
      p3 mustEqual p3s
      p4 mustEqual p4s

      // https://github.com/locationtech/spatial4j/blob/master/FORMATS.md beware, typo?
      // Rectangle	ENVELOPE(1, 2, 4, 3) (minX, maxX, maxY, minY)
      // https://github.com/locationtech/spatial4j/blob/master/src/main/java/org/locationtech/spatial4j/io/WKTReader.java#L245
      val bboxWkt = f"ENVELOPE($east, $west, $north, $south)"
      val bbox = shpReader.read(bboxWkt).asInstanceOf[Rectangle]

      // but rect builder method is logical
      // Rectangle rect(double minX, double maxX, double minY, double maxY);
      val bboxs = ctx.getShapeFactory().rect(east, west, south, north )

      bbox mustEqual bboxs

      val calc1 = new BBoxCalculator(ctx)
      calc1.expandRange(east, west, south, north)
      val rect1 = calc1.getBoundary

      rect1 mustEqual bboxs

      // BBoxCalculator tests
      val calc2 = new BBoxCalculator(ctx)
      calc2.expandRange(p1.getBoundingBox)
      calc2.expandRange(p2.getBoundingBox)
      calc2.expandRange(p3.getBoundingBox)
      calc2.expandRange(p4.getBoundingBox)
      val rect2 = calc2.getBoundary

      // BBoxCalculator expandRange with point bboxes seems to calculate overall bbox wrong from only source points?
      rect2 must not equal bboxs

      val points: java.util.List[Point] = new ArrayList[Point](4)
      points.add(p1)
      points.add(p2)
      points.add(p3)
      points.add(p4)
      val shapeCollection = new ShapeCollection(points, ctx)

      // shapecollection will therefore also fail, because uses internally bboxcalculatr as above
      shapeCollection.getBoundingBox must not equal bboxs

      shapeCollection.getBoundingBox mustEqual rect2
    }

    "should compute date, times and datetimes" in {
      // Play 2.5 requires Java 8
      // Java8 new date types

      val gmdDateStamp = <gmd:dateStamp><gco:Date>2012-12-20</gco:Date></gmd:dateStamp>
      val gmdDateType = <gmd:date><gmd:CI_Date><gmd:date><gco:Date>2016-05-17</gco:Date></gmd:date></gmd:CI_Date></gmd:date>
      val emptyDate = <gmd:dateStamp><gco:Date/></gmd:dateStamp>

      val dateString1 = ( gmdDateStamp \\ "dateStamp" \ "Date" ).text
      val dateString2 = ( gmdDateType \\ "date" \ "CI_Date" \ "date" \ "Date").text
      val dateString3 = ( emptyDate \\ "dateStamp" \ "Date" ).text

      val localDate1 = java.time.LocalDate.of(2012, Month.DECEMBER, 20)
      val localDate2 = java.time.LocalDate.of(2016, Month.MAY, 17)
      val localTime = LocalTime.of(12, 0)
      val auckland = ZonedDateTime.of(localDate1, localTime, ZoneId.of("Pacific/Auckland"))

      val oneDayLater = auckland.plusDays(1)
      val duration = Duration.between(auckland, oneDayLater)

      println("ISO_LOCAL_DATE: " + auckland.format(DateTimeFormatter.ISO_LOCAL_DATE))
      println("ISO_DATE: " + auckland.format(DateTimeFormatter.ISO_DATE))
      println("ISO_OFFSET_DATE_TIME: " + auckland.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
      println("ISO_DATE_TIME: " + auckland.format(DateTimeFormatter.ISO_DATE_TIME))
      println("RFC_1123_DATE_TIME: " + auckland.format(DateTimeFormatter.RFC_1123_DATE_TIME))

      val parsedDate1 = LocalDate.parse(dateString1, DateTimeFormatter.ISO_LOCAL_DATE)
      val parsedDate2 = LocalDate.parse(dateString2, DateTimeFormatter.ISO_LOCAL_DATE)

      val thrown = the [DateTimeParseException] thrownBy LocalDate.parse(dateString3, DateTimeFormatter.ISO_LOCAL_DATE)
      thrown.getMessage must equal ("Text '' could not be parsed at index 0")

      parsedDate1 mustEqual localDate1
      parsedDate2 mustEqual localDate2
    }
  }
}

