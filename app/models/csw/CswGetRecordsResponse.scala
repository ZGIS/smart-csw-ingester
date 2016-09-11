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

package models.csw

import java.time.format.DateTimeFormatter
import utils.StringUtils._

import scala.xml.Elem

/**
  * Holds a GetRecordsResponse.
  *
  * @param xml
  */
class CswGetRecordsResponse(val xml: Elem) {
  //Parse XML
  val (numberOfRecordsReturned: Int, numberOfRecordsMatched: Int, nextRecord: Int) =
    xml.label match {
      case "GetRecordsResponse" => {
        ((xml \ "SearchResults" \@ "numberOfRecordsReturned").toIntWithDefault(),
         (xml \ "SearchResults" \@ "numberOfRecordsMatched").toIntWithDefault(),
         (xml \ "SearchResults" \@ "nextRecord").toIntWithDefault())
      }
      case _ => throw new IllegalArgumentException(f"Expected Root Element <csw:GetRecordsResponse> but found ${xml.label}")
    }
}

/**
  * CswGetRecordsResponse companion object
  */
object CswGetRecordsResponse {
  /**
    * creates CswGetRecordsResponse from XML
    *
    * @param xml
    * @return
    */
  def apply(xml: Elem): CswGetRecordsResponse = {
    new CswGetRecordsResponse(xml)
  }

  /**
    * creates empty CswGetRecordsResponse
    *
    * @return
    */
  def empty(): CswGetRecordsResponse = {
    CswGetRecordsResponse(
      <csw:GetRecordsResponse xsi:schemaLocation="http://www.opengis.net/cat/csw/2.0.2 http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd"
                              version="2.0.2" xmlns:sitemap="http://www.sitemaps.org/schemas/sitemap/0.9"
                              xmlns:csw="http://www.opengis.net/cat/csw/2.0.2">
        <csw:SearchStatus timestamp={java.time.LocalDateTime.now().format(DateTimeFormatter.ISO_INSTANT)}/>
        <csw:SearchResults elementSet="full" recordSchema="http://www.isotc211.org/2005/gmd"
                           numberOfRecordsReturned="0" numberOfRecordsMatched="0" nextRecord="0">
        </csw:SearchResults>
      </csw:GetRecordsResponse>
    )
  }

}
