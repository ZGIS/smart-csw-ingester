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

package models.gmd

import java.net.URL

import utils.ClassnameLogger
import info.smart.models.owc100.UrlFormatOfferingExtensions

sealed trait OfferingType {
  def code: URL
}

object OfferingType extends ClassnameLogger {

  case object WMS extends OfferingType {
    val code = new URL(s"$URLTEMPLATE/wms")
  }

  case object WFS extends OfferingType {
    val code = new URL(s"$URLTEMPLATE/wfs")
  }

  case object WCS extends OfferingType {
    val code = new URL(s"$URLTEMPLATE/wcs")
  }

  case object WPS extends OfferingType {
    val code = new URL(s"$URLTEMPLATE/wps")
  }

  case object CSW extends OfferingType {
    val code = new URL(s"$URLTEMPLATE/csw")
  }

  case object GEOTIFF extends OfferingType {
    val code = new URL(s"$URLTEMPLATE/geotiff")
  }

  case object SOS extends OfferingType {
    val code = new URL(s"$URLTEMPLATE/sos")
  }

  private val fmt = new UrlFormatOfferingExtensions

  private val URLTEMPLATE = fmt.GENERIC_OFFERING_URL_TEMPLATE

}
