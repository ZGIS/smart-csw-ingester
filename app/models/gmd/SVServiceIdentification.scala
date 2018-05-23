/*
 * Copyright (c) 2011-2017 Interfaculty Department of Geoinformatics, University of
 * Salzburg (Z_GIS) & Institute of Geological and Nuclear Sciences Limited (GNS Science)
 * in the SMART Aquifer Characterisation (SAC) programme funded by the New Zealand
 * Ministry of Business, Innovation and Employment (MBIE) and Department of Geography,
 * University of Tartu, Estonia (UT) under the ETAG Mobilitas Pluss grant No. MOBJD233.
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


package models.gmd

import java.net.URL

import info.smart.models.owc100.UrlFormat
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils.ClassnameLogger

import scala.xml.NodeSeq

object SVServiceIdentification extends ClassnameLogger {

  /*
  <srv:coupledResource>
                <srv:SV_CoupledResource>
                    <srv:operationName>
                        <gco:CharacterString>GetObservation</gco:CharacterString>
                    </srv:operationName>
                    <srv:identifier>
                        <gco:CharacterString>http://vocab.smart-project.info/ngmp/offering/1616
                        </gco:CharacterString>
                    </srv:identifier>
                </srv:SV_CoupledResource>
            </srv:coupledResource>
   */

  def coupledResourceToLinkage(nodeSeq: NodeSeq): CIOnlineResource = ???
}
