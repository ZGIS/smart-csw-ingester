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

import utils.ClassnameLogger

/**
  * Simple Request Builder for GetRecords-Request.
  */
object CswGetRecordsRequest extends ClassnameLogger {
  def apply(startPosition: Int, maxDocuments: Int): String = {
    val result =
      f"""
         |<csw:GetRecords xmlns:csw="http://www.opengis.net/cat/csw/2.0.2"
         |                xmlns:ogc="http://www.opengis.net/ogc"
         |                xmlns:gmd="http://www.isotc211.org/2005/gmd"
         |                service="CSW" version="2.0.2"
         |                resultType="results" startPosition="${startPosition}" maxRecords="${maxDocuments}"
         |                outputFormat="application/xml" outputSchema="http://www.isotc211.org/2005/gmd"
         |                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |                xsi:schemaLocation="http://www.opengis.net/cat/csw/2.0.2
         |                                    http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd">
         |  <csw:Query typeNames="gmd:MD_Metadata">
         |    <csw:ElementSetName>full</csw:ElementSetName>
         |  </csw:Query>
         |</csw:GetRecords>
      """.stripMargin
    logger.debug(f"GetRecords Request: ${result}")
    result
  }
}
