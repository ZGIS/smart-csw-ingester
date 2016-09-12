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

import java.time.LocalDate

import models.gmd.{GeoJSONFeatureCollectionWriter, MdMetadataSet, MdMetadataSetWriter}
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.shape._
import org.scalatest.Ignore
import org.scalatestplus.play.PlaySpec
import play.api.libs.json._

import scala.xml._

/**
  * Test Spec for [[MdMetadataSet]]
  */
class MdMetadataSetSpec extends PlaySpec {
  private lazy val ctx = SpatialContext.GEO

  "MdMetadataSet " can {
    lazy val xmlResource = this.getClass().getResource("sac_rs_ewt_metadata.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElement = MdMetadataSet.fromXml(xml)

    val defaultDate = LocalDate.of(2012, 1, 1)

    "parse ISO_LOCAL_DATE: 2012-01-01 correctly " in {
      MdMetadataSet.dateFromStrings(List("2012-01-01")) mustEqual defaultDate
    }

    "parse ISO_LOCAL_DATE_TIME, 2012-01-01T10:15:30 correctly " in {
      MdMetadataSet.dateFromStrings(List("2012-01-01T10:15:30")) mustEqual defaultDate
    }

    "parse pattern \"yyyy-MM\", 2012-01 correctly " in {
      MdMetadataSet.dateFromStrings(List("2012-01")) mustEqual defaultDate
    }

    "parse pattern \"yyyy\", 2012 correctly " in {
      MdMetadataSet.dateFromStrings(List("2012-01")) mustEqual defaultDate
    }

    "parse BASIC_ISO_DATE, 20120101 correctly " in {
      MdMetadataSet.dateFromStrings(List("20120101")) mustEqual defaultDate
    }

    "parse ISO_INSTANT, 2012-01-01T10:15:30Z correctly " in {
      MdMetadataSet.dateFromStrings(List("2012-01-01T10:15:30Z")) mustEqual defaultDate
    }

    "parse ISO_OFFSET_DATE, 2012-01-01+13:00 correctly " in {
      MdMetadataSet.dateFromStrings(List("2012-01-01+13:00")) mustEqual defaultDate
    }

    "parse ISO_OFFSET_DATE_TIME, 2012-01-01T10:15:30+13:00 correctly " in {
      MdMetadataSet.dateFromStrings(List("2012-01-01T10:15:30+13:00")) mustEqual defaultDate
    }

    "parse EMPTY date to 1970-01-01" in {
      MdMetadataSet.dateFromStrings(List("")) mustEqual LocalDate.ofEpochDay(0)
    }

    "should parse No dateStamp/Date and find date/CI_Date/date/Date MdMetadataSet" in {

      parsedElement mustBe defined
      parsedElement.get.dateStampAsIsoString mustEqual ("2016-05-17")
    }
  }

  "MD_Metadata_COMPLETE.xml" must {
    lazy val xmlResource = this.getClass().getResource("MD_Metadata_COMPLETE.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElementOption = MdMetadataSet.fromXml(xml, "linz")

    "parse without errors" in {
      parsedElementOption mustBe defined
    }

    "have correctly parsed values" in {
      val parsedElement = parsedElementOption.get
      parsedElement.fileIdentifier mustEqual ("294f127b-addb-24d8-0df1-f014032dcd02")
      parsedElement.dateStampAsIsoString mustEqual ("2015-04-08")
      parsedElement.title mustEqual ("NZ Beacon Points (Topo, 1:250k)")
      parsedElement.abstrakt.length mustEqual (599) //dont wanna copy&paste the whole abstract
      parsedElement.keywords mustEqual (List("New Zealand"))
      parsedElement.topicCategory mustEqual (List("imageryBaseMapsEarthCover"))
      parsedElement.contactName mustEqual ("omit, Omit")
      parsedElement.contactOrg mustEqual ("LINZ - Land Information New Zealand, LINZ - Land Information New Zealand, ANZLIC the Spatial Information Council")
      parsedElement.contactEmail mustEqual ("info@linz.govt.nz, info@linz.govt.nz")
      parsedElement.license must endWith ("Released under Creative Commons By")
      parsedElement.bbox mustEqual (ctx.getShapeFactory.rect(168.360399911, 178.548442791, -46.6603664299,
        -34.1537940929))
      parsedElement.origin mustEqual "linz"
    }
  }

  "Wrong BBoxes" must {
    lazy val xmlResource = this.getClass().getResource("MD_Metadata_NO_BBOX.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElement = MdMetadataSet.fromXml(xml, "linz")
    lazy val world = ctx.getShapeFactory().rect(-180.0, 180.0, -90.0, 90.0)

    "MD_Metadata_NO_BBOX.xml parse without errors" in {
      parsedElement mustBe defined
    }

    //TODO SR check all other values too?
    "MD_Metadata_NO_BBOX.xml must have WORLD bounding box" in {
      parsedElement.get.bbox mustEqual world
    }

    // east: Double, west: Double, south: Double, north: Double
    "cut too large coordinates into WORLD bounding box" in {
      MdMetadataSet.bboxFromCoords(-190.0, 180.0, -90.0, 90.0) mustEqual world
      MdMetadataSet.bboxFromCoords(-180.0, 190.0, -90.0, 90.0) mustEqual world
      MdMetadataSet.bboxFromCoords(-180.0, 180.0, -95.0, 90.0) mustEqual world
      MdMetadataSet.bboxFromCoords(-180.0, 180.0, -90.0, 95.0) mustEqual world
    }

    "switch north / south to correct order " in {
      MdMetadataSet.bboxFromCoords(-180.0, 180.0, 90.0, -90.0) mustEqual world
    }

    "handle zero width longitude edge cases" in {
      val (west, east) = MdMetadataSet.pruneLongitudeValues(180, -180)
      west mustBe 180
      east mustBe -180

      val mirrorWorld = ctx.getShapeFactory().rect(180, -180, -90.0, 90.0)

      MdMetadataSet.bboxFromCoords(180, -180, -90.0, 90.0) mustEqual mirrorWorld
    }

    "handle reversed / date line wrap longitude cases" in {

      val (prunedWest, prunedEast) = MdMetadataSet.pruneLongitudeValues(166.6899599, -176.176448433)
      prunedWest mustEqual 166.6899599
      prunedEast mustEqual -176.176448433

      val dateWrapped = ctx.getShapeFactory().rect(166.6899599, -176.176448433, -90.0, 90.0)

      MdMetadataSet.bboxFromCoords(166.6899599, -176.176448433, -90.0, 90.0) mustEqual dateWrapped
    }
  }

  "JSON writer" should {
    implicit val mdMetadataSetWrite = MdMetadataSetWriter
    implicit val geoJSONFeatureCollectionWrite = GeoJSONFeatureCollectionWriter

    lazy val xmlResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
    lazy val xmlResource2 = this.getClass().getResource("sac_rs_ewt_metadata.xml")
    lazy val xml1 = scala.xml.XML.load(xmlResource1)
    lazy val xml2 = scala.xml.XML.load(xmlResource2)
    lazy val parsedElement1 = MdMetadataSet.fromXml(xml1, "linz")
    lazy val parsedElement2 = MdMetadataSet.fromXml(xml2, "smart")

    "parse without errors" in {
      parsedElement1 mustBe defined
      parsedElement2 mustBe defined

      Json.toJson(parsedElement1.get).toString() must include ("linz")
      Json.toJson(parsedElement2.get).toString() must include ("smart")
    }

    "build JsArrList of GeoJSON Features from List of MdMetadataSet" in {
      val gmdList = List(parsedElement1.get, parsedElement2.get)
      val listOfGeoJsonFeatures = Json.toJson(gmdList)
      (listOfGeoJsonFeatures \\ "fileIdentifier").size mustBe 2
    }

    "provide GeoJSON Feature for one MdMetadaset" in {
      val jsResource = this.getClass().getResource("linzFeatureTest.json")
      val jsonTestFeature = scala.io.Source.fromURL(jsResource).getLines.mkString
      Json.toJson(parsedElement1.get) mustEqual Json.parse(jsonTestFeature)
    }

    "provide List of GeoJSON as FeatureCollection for list of MdMetadaset" in {
      val gmdList = List(parsedElement1.get, parsedElement2.get)

      val geoJsonFeatureCollection = Json.toJson(gmdList)

      val jsResource = this.getClass().getResource("featureCollectionTest.json")
      val jsonTestFeatureCollection = scala.io.Source.fromURL(jsResource).getLines.mkString
      geoJsonFeatureCollection mustEqual Json.parse(jsonTestFeatureCollection)

    }

    "survive empty result list with FeatureCollection and feature count 0" in {
      val gmdList = List[MdMetadataSet]()
      val listOfGeoJsonFeatures = Json.toJson(gmdList)
      (listOfGeoJsonFeatures \\ "type").map(_.as[String]).filter(str => str.equalsIgnoreCase("FeatureCollection")).size mustBe 1

      val count = listOfGeoJsonFeatures \\ "count"
      count.size mustBe 1
      count.headOption.getOrElse(JsNumber(666)) mustEqual JsNumber(0)
    }

    "cannot yet insert an additional key/value pair in the featurecollection header" in {

      import play.api.libs.json._ // JSON library
      import play.api.libs.json.Reads._ // Custom validation helpers
      import play.api.libs.functional.syntax._ // Combinator syntax

      val jsResource = this.getClass().getResource("featureCollectionTest.json")
      val jsonTestFeatureCollection = scala.io.Source.fromURL(jsResource).getLines.mkString
      val geoJsonFeatureCollection = Json.parse(jsonTestFeatureCollection)

      // should use a JsonTransformer

    }

  }

  @Ignore def `test: Parsing Notes for Alex describe should`: Unit = {

    val xmlResource = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
    val xml: scala.xml.NodeSeq = scala.xml.XML.load(xmlResource)
    val nsGmd = "http://www.isotc211.org/2005/gmd"
    val nsGco = "http://www.isotc211.org/2005/gco"

    //This basically tests the Scala XML Library and not our parser, but I need to keep these notes

    "work with namespaces " in {
      val n1: NodeSeq = (xml \ "fileIdentifier" \ "CharacterString")
      n1.filter(x => x.namespace == nsGmd).isEmpty mustBe (true)

      val n2: NodeSeq = (xml \ "fileIdentifier" \ "CharacterString")
      n2.filter(x => x.namespace == nsGco).size mustEqual 1
    }

    "work with codelists as attributes" in {
      val at1 = (xml \\ "identificationInfo" \ "MD_DataIdentification" \ "status" \ "MD_ProgressCode").filter(
        node => node.attribute("codeList")
          .exists(
            codeList => codeList.text == "http://asdd.ga.gov.au/asdd/profileinfo/gmxCodelists.xml#MD_ProgressCode"))
      at1.size mustEqual 1
    }

    "java.util.UUID not useful for reverse from fileidentifiers" in {
      val thrown = the[java.lang.IllegalArgumentException] thrownBy java.util.UUID.fromString(
        "urn:uuid:2c5f1309-d721-4299-88bf-e462c577b99a-horowhenua_ws:ewt_nzprj_new")
      thrown.getMessage mustBe ("Invalid UUID string: urn:uuid:2c5f1309-d721-4299-88bf-e462c577b99a-horowhenua_ws:ewt_nzprj_new")
    }

    "coordinates order of Retangle vs WKT" in {
      //This basically tests the ShapeFactory and not our parser, but I need to keep these notes

      // https://github.com/locationtech/spatial4j/blob/master/FORMATS.md beware, typo?
      // Rectangle	ENVELOPE(1, 2, 4, 3) (minX, maxX, maxY, minY)
      // https://github.com/locationtech/spatial4j/blob/master/src/main/java/org/locationtech/spatial4j/io/WKTReader.java#L245

      // rect builder method is logical
      // Rectangle rect(double minX, double maxX, double minY, double maxY);
      val bbox1 = ctx.getShapeFactory().rect(166.6899599, 176.176448433, -47.1549297167, -34.4322590833)

      val (prunedWest, prunedEast) = MdMetadataSet.pruneLongitudeValues(166.6899599, 176.176448433)
      val (prunedSouth, prunedNorth) = MdMetadataSet.pruneLatitudeValues(-47.1549297167, -34.4322590833)

      prunedWest mustEqual 166.6899599
      prunedEast mustEqual 176.176448433
      prunedSouth mustEqual -47.1549297167
      prunedNorth mustEqual -34.4322590833

      val bbox2 = MdMetadataSet.bboxFromCoords(166.6899599, 176.176448433, -47.1549297167, -34.4322590833)
      bbox2 mustEqual bbox1

      // DateLineWrap
      val bbox3 = ctx.getShapeFactory().rect(166.6899599, -176.176448433, -47.1549297167, -34.4322590833)

      // PoleWrap doesn't work, South always mist be smaller than North
      val thrown = the[org.locationtech.spatial4j.exception.InvalidShapeException] thrownBy
        ctx.getShapeFactory().rect(166.6899599, 176.176448433, -34.4322590833, -47.1549297167)
      thrown.getMessage mustBe ("maxY must be >= minY: -34.4322590833 to -47.1549297167")
    }
  }
}
