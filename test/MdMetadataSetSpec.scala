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

import models.gmd.MdMetadataSet
import org.locationtech.spatial4j.context.SpatialContext
import org.scalatestplus.play.PlaySpec

/**
  * Test Spec for [[MdMetadataSet]]
  */
class MdMetadataSetSpec extends PlaySpec {
  private lazy val ctx = SpatialContext.GEO

  "MdMetadataSet " can {
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
  }

  "MD_Metadata_COMPLETE.xml" must {
    lazy val xmlResource = this.getClass().getResource("MD_Metadata_COMPLETE.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElementOption = MdMetadataSet.fromXml(xml)

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
      parsedElement.license must endWith("Released under Creative Commons By")
      parsedElement.bbox mustEqual (ctx.getShapeFactory.rect(178.548442791, 168.360399911, -46.6603664299,
        -34.1537940929))
    }
  }

  "MD_Metadata_NO_BBOX.xml" must {
    lazy val xmlResource = this.getClass().getResource("MD_Metadata_NO_BBOX.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElement = MdMetadataSet.fromXml(xml)

    "parse without errors" in {
      parsedElement mustBe defined
    }

    //TODO SR check all other values too?

    "have WORLD bounding box" in {
      parsedElement.get.bbox mustEqual (ctx.getShapeFactory().rect(-180, 180, -90, 90))
    }
  }

  "should parse No dateStamp/Date and find date/CI_Date/date/Date GmdElementSet" in {
    lazy val xmlResource = this.getClass().getResource("sac_rs_ewt_metadata.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElement = MdMetadataSet.fromXml(xml)
    parsedElement mustBe defined
    parsedElement.get.dateStampAsIsoString mustEqual ("2016-05-17")
  }

  /*  SR does this test the same as above? Creates the same element once from XML and once by hand an then checks if equal?
      "should build format case classes" in {
      val ctx = SpatialContext.GEO
      val shpReader = ctx.getFormats().getReader(ShapeIO.WKT)
      val shpWriter = ctx.getFormats().getWriter(ShapeIO.GeoJSON)

      val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
      val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)

      val localDate = java.time.LocalDate.of(2012, Month.DECEMBER, 20)

      val gmdElem1 = MdMetadataSet.fromXml(xml1)

      val bbox = MdMetadataSet.bboxFromXml(xml1)
      val bboxs = ctx.getShapeFactory().rect(-176.176448433, 166.6899599, -47.1549297167, -34.4322590833)
      bbox mustEqual bboxs

      val gmdElem2 = MdMetadataSet("23bdd7a3-fd21-daf1-7825-0d3bdc256f9d",
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

      gmdElem1 mustBe defined
      gmdElem1.get.fileIdentifier mustEqual gmdElem2.fileIdentifier
      gmdElem1.get.dateStamp mustEqual gmdElem2.dateStamp
      gmdElem1.get.title mustEqual gmdElem2.title
      gmdElem1.get.abstrakt mustEqual gmdElem2.abstrakt
      gmdElem1.get.keywords mustEqual gmdElem2.keywords
      gmdElem1.get.topicCategory mustEqual gmdElem2.topicCategory
      gmdElem1.get.contactName mustEqual gmdElem2.contactName
      gmdElem1.get.contactOrg mustEqual gmdElem2.contactOrg
      gmdElem1.get.contactEmail mustEqual gmdElem2.contactEmail
      gmdElem1.get.license mustEqual gmdElem2.license
      gmdElem1.get.bbox mustEqual gmdElem2.bbox

      gmdElem1.get mustEqual gmdElem2

      gmdElem1.get.dateStampAsIsoString mustEqual ("2012-12-20")

      val thrown = the[java.lang.IllegalArgumentException] thrownBy java.util.UUID.fromString(
        "urn:uuid:2c5f1309-d721-4299-88bf-e462c577b99a-horowhenua_ws:ewt_nzprj_new")
      thrown.getMessage mustBe ("Invalid UUID string: urn:uuid:2c5f1309-d721-4299-88bf-e462c577b99a-horowhenua_ws:ewt_nzprj_new")

      implicit val gmdElementSetWrite = MdMetadataSetWriter

      val gmdList = List(gmdElem1.get, gmdElem2)
      // println(gmdElem1.toString())
      // println(Json.toJson(gmdElem2))
      // println(Json.toJson(gmdList))
      val jsList = Json.toJson(gmdList)
      (jsList \\ "fileIdentifier").size mustBe 2

      val textJson = """{"fileIdentifier":"23bdd7a3-fd21-daf1-7825-0d3bdc256f9d","dateStamp":"2012-12-20","title":"NZ Primary Road Parcels","abstrakt":"This layer provides the **current** road parcel polygons with ...","keywords":["New Zealand"],"topicCategory":["boundaries","planningCadastre"],"contactName":"omit, Omit","contactOrg":"LINZ - Land Information New Zealand, LINZ - Land Information New Zealand, ANZLIC the Spatial Information Council","contactEmail":"info@linz.govt.nz, info@linz.govt.nz","license":"Crown copyright reserved, Released under Creative Commons By with: Following Disclaimers..., Crown copyright reserved, Released under Creative Commons By","bbox":[-176.176448433,166.6899599,-34.4322590833,-47.1549297167],"origin":""}"""
      Json.toJson(gmdElem2).toString() mustEqual (textJson)
    }*/
}
