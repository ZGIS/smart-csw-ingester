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

  "Parsing BBoxes" must {
    lazy val xmlResource = this.getClass().getResource("MD_Metadata_NO_BBOX.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElement = MdMetadataSet.fromXml(xml, "linz")
    lazy val world = ctx.getShapeFactory().rect(-180.0, 180.0, -90.0, 90.0)

    "cut off invalid north / south values " in {
      val (south, north) = MdMetadataSet.pruneLatitudeValues(-123, 123)
      north mustEqual 90
      south mustEqual -90
      MdMetadataSet.bboxFromCoords(-180.0, 180.0, 123.0, -123.0) mustEqual world
    }

    "switch north / south to correct order " in {
      val (south, north) = MdMetadataSet.pruneLatitudeValues(80, -70)
      north mustEqual 80
      south mustEqual -70

      val (south2, north2) = MdMetadataSet.pruneLatitudeValues(123, -123)
      north2 mustEqual 90
      south2 mustEqual -90

      val northSouthSwitch= ctx.getShapeFactory().rect(-180, 180, -70.0, 80.0)
      MdMetadataSet.bboxFromCoords(-180.0, 180.0, 80.0, -70.0) mustEqual northSouthSwitch
      MdMetadataSet.bboxFromCoords(-180.0, 180.0, 123.0, -123.0) mustEqual world
    }

    "handle zero width longitude edge cases" in {
      val (west, east) = MdMetadataSet.pruneLongitudeValues(180, -180)
      west mustBe 180
      east mustBe -180

      val mirrorWorld = ctx.getShapeFactory().rect(180, -180, -90.0, 90.0)

      MdMetadataSet.bboxFromCoords(180, -180, -90.0, 90.0) mustEqual mirrorWorld
    }

    "cut to large east -> west into world" in {
      MdMetadataSet.bboxFromCoords(-190.0, 180.0, -90.0, 90.0) mustEqual world
      MdMetadataSet.bboxFromCoords(-180.0, 190.0, -90.0, 90.0) mustEqual world
      MdMetadataSet.bboxFromCoords(-360.0, 10.0, -90.0, 90.0) mustEqual world
      MdMetadataSet.bboxFromCoords(-10.0, 360.0, -90.0, 90.0) mustEqual world
    }

    "handle date line wrap longitude cases" should {
      //small box over dateline
      val dateWrapped = ctx.getShapeFactory().rect(170, -170, -90.0, 90.0)
      "transform (170,-170) -> (170,-170)" in {
        MdMetadataSet.bboxFromCoords(170, -170, -90.0, 90.0) mustEqual dateWrapped
        MdMetadataSet.bboxFromCoords(170, -170, -90.0, 90.0).getCrossesDateLine mustEqual true
      }

      //same box but with coordinate >180
      "transform (170,190) -> (170,-170)" in {
        MdMetadataSet.bboxFromCoords(170, 190, -90.0, 90.0) mustEqual dateWrapped
        MdMetadataSet.bboxFromCoords(170, 190, -90.0, 90.0).getCrossesDateLine mustEqual true
      }

      //same box but with coordinate <-180
      "transform (-190,-170) -> (170,-170)" in {
        MdMetadataSet.bboxFromCoords(-190, -170, -90.0, 90.0) mustEqual dateWrapped
        MdMetadataSet.bboxFromCoords(-190, -170, -90.0, 90.0).getCrossesDateLine mustEqual true
      }

    }

    "MD_Metadata_NO_BBOX.xml parse without errors" in {
      parsedElement mustBe defined
    }

    "MD_Metadata_NO_BBOX.xml must have WORLD bounding box" in {
      parsedElement.get.bbox mustEqual world
    }

  }

  //TODO SR delete this. its a leftover from ancient times
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
