import java.io.{FileInputStream, InputStream}

import models.gmd.{CIOnlineResource, ResourceType}
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.Json
import utils.ClassnameLogger

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

/**
  * Test suite for [[models.gmd.CIOnlineResource]]
  */
class CIOnlineResourceSpec extends PlaySpec with ClassnameLogger {

  val xmlResources = (
    this.getClass().getResource("CIOnlineResource/ciOnlineResourceFull.xml") ::
      this.getClass().getResource("CIOnlineResource/ciOnlineResourceMinimal.xml") ::
      this.getClass().getResource("CIOnlineResource/ciOnlineResourceGeogovt.xml") ::
      this.getClass().getResource("CIOnlineResource/ciOnlineResourceProtocolMetadata01.xml") ::
      Nil
    ).map(scala.xml.XML.load(_))

  val jsonResources = (
    this.getClass().getResource("CIOnlineResource/ciOnlineResourceFull.json") ::
      this.getClass().getResource("CIOnlineResource/ciOnlineResourceMinimal.json") ::
      this.getClass().getResource("CIOnlineResource/ciOnlineResourceGeogovt.json") ::
      this.getClass().getResource("CIOnlineResource/ciOnlineResourceProtocolMetadata01.json") ::
      Nil
    ).map(foo => Json.parse(foo.openStream()))


  "CIOnlineResource" should {
    "parse full XML" in {
      val ciOnlineResource = CIOnlineResource.fromXml(xmlResources(0), "test")
      ciOnlineResource.linkage mustEqual "https://www.servername.com/resource/index.html"
      ciOnlineResource.name mustEqual Some("Name of the resource")
      ciOnlineResource.description mustEqual Some("Description of the resource")
      ciOnlineResource.resourceType mustEqual ResourceType.WEBSITE
    }

    "parse minimal XML" in {
      val ciOnlineResource = CIOnlineResource.fromXml(xmlResources(1), "test")
      ciOnlineResource.linkage mustEqual "https://www.servername.com/resource/index.html"
      ciOnlineResource.name mustEqual None
      ciOnlineResource.description mustEqual None
      ciOnlineResource.resourceType mustEqual ResourceType.WEBSITE
    }

    "parse Geogovt XML" in {
      val ciOnlineResource = CIOnlineResource.fromXml(xmlResources(2), "test")
      ciOnlineResource.linkage mustEqual "http://geoportal.doc.govt.nz/ArcGIS/rest/services/GeoportalServices/DOC_Huts/MapServer"
      ciOnlineResource.name mustEqual None
      ciOnlineResource.description mustEqual None
      ciOnlineResource.resourceType mustEqual ResourceType.METADATA
    }

    "parse / interpret protocols in XML correctly " in {
      val ciOnlineResource = CIOnlineResource.fromXml(xmlResources(3), "test")
      ciOnlineResource.linkage mustEqual "https://www.servername.com/resource/index.html"
      ciOnlineResource.name mustEqual None
      ciOnlineResource.description mustEqual None
      ciOnlineResource.resourceType mustEqual ResourceType.METADATA
    }

    "write JSON correctly" in {
      xmlResources
        .map(CIOnlineResource.fromXml(_, "test"))
        .map(Json.toJson(_)) mustEqual jsonResources
    }

    "read JSON correctly" in {
      xmlResources.map(CIOnlineResource.fromXml(_, "test")) mustEqual
            jsonResources.map(Json.fromJson[CIOnlineResource](_).get)
    }
  }
}
