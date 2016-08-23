import models.GmdElementSet
import org.scalatestplus.play.PlaySpec

import scala.xml.Elem

/*
 * Copyright (c) 2011-2017 Interfaculty Department of Geoinformatics, University of Salzburg (Z_GIS)
 *                       & Institute of Geological and Nuclear Sciences Limited (GNS Science)
 *                       in the SMART Aquifer Characterisation (SAC) programme funded by the New Zealand
 *                       Ministry of Business, Innovation and Employment (MBIE)
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
 *
 *
 */
/**
  * Created by steffen on 23.08.16.
  */
class GmdElementSetSpec extends PlaySpec {

  "should parse COMPLETE GmdElementSet " in {
    val asResource = this.getClass().getResource("MD_Metadata_COMPLETE.xml")
    val xml = scala.xml.XML.load(asResource)
    val parsedElement = GmdElementSet.fromXml(xml)
    parsedElement.fileIdentifier mustEqual("294f127b-addb-24d8-0df1-f014032dcd02")
  }

  "should parse NO_BBOX GmdElementSet " in {
    val asResource = this.getClass().getResource("MD_Metadata_NO_BBOX.xml")
    val xml = scala.xml.XML.load(asResource)
    val parsedElement = GmdElementSet.fromXml(xml)
    parsedElement.fileIdentifier mustEqual("4a5021fa-b85d-cdb7-c6aa-53ef470da713")
  }

}
