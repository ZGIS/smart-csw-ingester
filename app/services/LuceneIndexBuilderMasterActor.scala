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

package services

import javax.inject.Inject

import akka.actor.{Actor, ActorRef}
import play.api.libs.concurrent.InjectedActorSupport
import services.LuceneIndexBuilderMasterActor.GetSpecificIndexBuilder

object LuceneIndexBuilderMasterActor {

  case class GetSpecificIndexBuilder(catalogueName: String)

}

/** at that point just a vehicle to get an injected child actor
  * TODO put a "create index" here
  */
class LuceneIndexBuilderMasterActor @Inject()(luceneIndexBuilderActorFactory: LuceneIndexBuilderActor.Factory) extends Actor with
  InjectedActorSupport {
  override def receive: Receive = {
    case GetSpecificIndexBuilder(catalogueName) => {
      val indexBuilderActor: ActorRef = injectedChild(luceneIndexBuilderActorFactory(), s"indexBuilder-$catalogueName")
      sender() ! indexBuilderActor
    }
  }
}
