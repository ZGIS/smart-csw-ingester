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

package actors

import javax.inject._

import akka.actor._
import akka.event.Logging
import play.api.Configuration

// test from https://www.playframework.com/documentation/2.5.x/ScalaAkka
object IndexActor {

  case class GetConfig(configuration: Configuration)
  case object RefreshIndex

}

class IndexActor @Inject()() extends Actor {
  import IndexActor.GetConfig

  val log = Logging(context.system, this)

  def receive = {
    case GetConfig =>
      sender() ! "will initialise"
  }
}