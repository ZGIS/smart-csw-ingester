/*
 * Copyright (c) 2011-2017 Interfaculty Department of Geoinformatics, University of Salzburg (Z_GIS)
 * & Institute of Geological and Nuclear Sciences Limited (GNS Science)
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
 *
 *
 */

package utils

/**
  * Adds some convenience functions to Strings
  */
object StringUtils {

  /**
    * Safe number converters that provide default values on [[NumberFormatException]]
    *
    * @param s
    */
  implicit class NumberConverters(val s: String) {

    import scala.util.control.Exception._
    /** catcher for [[NumberFormatException]] */
    private lazy val nfeCatcher = catching(classOf[NumberFormatException])

    /**
      * Converts [[String]] to Int with default value on [[NumberFormatException]].
      *
      * @param default default value on NumberFormatException. Defaults to 0 if omitted.
      * @return
      */
    def toIntWithDefault(default: Int = 0): Int = {
      nfeCatcher.opt(s.toInt).getOrElse(default)
    }

    /**
      * Converts [[String]] to [[Double]] with default value on [[NumberFormatException]].
      *
      * @param default default value on NumberFormatException. Defaults to 0.0 if omitted.
      * @return
      */
    def toDoubleWithDefault(default: Double = 0.0): Double = {
      nfeCatcher.opt(s.toDouble).getOrElse(default)
    }
  }

}
