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

import java.net.URL

import models.gmd.MdMetadataSet._
import models.gmd._
import org.locationtech.spatial4j.context.SpatialContext
import org.scalatestplus.play.PlaySpec
import utils.OwcGeoJsonConverters

/**
  * Test Spec for [[SVServiceIdentification]]
  */
class SVServiceIdentificationSpec extends PlaySpec {
  private lazy val ctx = SpatialContext.GEO

  val origin = "smart"
  val originUrl = "https://portal.smart-project.info/pycsw/csw"

  "srv-ServiceIdentification.xml" must {
    lazy val xmlResource = this.getClass().getResource("srv-ServiceIdentification.xml")
    lazy val xml = scala.xml.XML.load(xmlResource)
    lazy val parsedElementOption = MdMetadataSet.fromXml(xml, origin, originUrl)

    "parse without errors" in {
      parsedElementOption mustBe defined

      val nodeSeq = xml
      val linkage = linkageFromXml(nodeSeq, origin)

      println(linkage.map(l => l.name + " - " +l.description.getOrElse("") + " - " +l.resourceType.toString + " - " +l.protocol + " - " +l.linkage.toString).mkString("::"))
      linkage.length >  0 mustBe true
    }

    "have correctly parsed values" in {

      val parsedElement = parsedElementOption.get
      parsedElement.fileIdentifier mustEqual ("urn:uuid:e4f8643b-2c5e-4727-9246-4ba482b742e6")
      parsedElement.hierarchyLevel mustEqual ("service")
      parsedElement.dateStampAsIsoString mustEqual ("1970-01-01")
      parsedElement.title mustEqual ("NGMP SOS")
      parsedElement.abstrakt mustEqual ("A SOS Instance for NGMP Groundwater Monitoring Data")
      parsedElement.keywords mustEqual (List("NGMP", "groundwater", "hydrogeology"))
      parsedElement.contactName mustEqual ("Alex kmoch")
      parsedElement.contactOrg mustEqual ("GNS Science / Z_GIS SMART")
      parsedElement.contactEmail mustEqual ("a.kmoch@gns.cri.nz")
      parsedElement.bbox mustEqual (ctx.getShapeFactory.rect(168.06, 177.98, -46.31, -34.85))
      parsedElement.origin mustEqual "smart"
    }

    "test new OGC service offerings" in {

      val url = new URL("http://www.some-server-com")

      //      val p0 = "OGC:WMS"
      //      val p0_1 = "ogc:WMS"
      val p1 = "OGC:WCS-1.0.0-http-get-capabilities"
      //      val p2 = "OGC:WCS-1.0.0-http-get-coverage"
      //      val p3 = "OGC:WMS-1.3.0-http-get-map"
      //      val p4 = "OGC:WFS-2.0.0-http-get-feature"
      //      val p5 = "OGC:SOS-1.0.0-http-get-observation"

      val offerings = OwcGeoJsonConverters.generateOwcOfferingFromServiceProto(p1, url)
      offerings.length mustBe 1

      offerings(0).operations.length mustBe 1
      println(offerings(0).operations(0).requestUrl.toString)
      offerings(0).operations(0).code mustEqual "GetCapabilities"
    }

    "test new offerings for ArcGIS" in {

      val url = new URL("http://geoportal.doc.govt.nz/ArcGIS/rest/services/GeoportalServices/DOC_Operations_Regions/MapServer")

      val requestUrl = new URL("http://geoportal.doc.govt.nz/arcgis/services/GeoportalServices/DOC_Operations_Regions/MapServer/WFSServer?request=GetCapabilities&service=WFS")
      val offerings = OwcGeoJsonConverters.generateOwcOfferingForArcServer(url)

      offerings.length mustBe 2

      offerings(0).operations.length mustBe 1
      println(offerings(0).operations(0).requestUrl.toString)
      offerings(0).operations(0).code mustEqual "GetCapabilities"
      offerings.filter(p => p.code.toString.contains("wfs")).head.operations(0).requestUrl mustEqual requestUrl
    }
  }
}
