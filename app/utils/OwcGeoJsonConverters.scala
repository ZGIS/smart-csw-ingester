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

package utils

import java.net.{URL, URLEncoder}
import java.time._

import info.smart.models.owc100.OwcOfferingType.{CSW, WFS, WMS}
import info.smart.models.owc100._
import models.gmd.{CIOnlineResource, GeoJSONFeatureCollectionWriter, MdMetadataSet, ResourceType}

object OwcGeoJsonConverters extends ClassnameLogger {

  /**
    * Converts to a Option[OwcLink].
    * OWC:Context specReference attribute: atom rel="profile" - geojson links.profiles[] array
    * OWC:Context contextMetadata attribute: atom rel="via" - geojson links.via[] array
    *
    * OWC:Resource contentDescription attribute: atom rel="alternate" - geojson links.alternates[] array
    * OWC:Resource preview attribute: atom rel="icon" - geojson links.previews[] array
    * OWC:Resource contentByRef attribute: atom rel="enclosure" - geojson links.data[] array
    * OWC:Resource resourceMetadata attribute: atom rel="via" - geojson links.via[] array
    *
    * links for data and previews (aka rels enclosure and icon should have length attributes set)
    *
    * @return Some[OwcLink] or None if CIOnlineResource could not be converted sensibly
    */
  def asOwcLink(cIOnlineResource: CIOnlineResource): Option[OwcLink] = {
    import utils.StringUtils._

    cIOnlineResource.protocol match {
      case Some("WWW:LINK-1.0-http--metadata-URL") =>
        Some(OwcLink(rel = "via",
          mimeType = Some("application/xml"),
          href = cIOnlineResource.linkage,
          title = cIOnlineResource.name))
      case Some("WWW:LINK-1.0-http--link") =>
        Some(OwcLink(rel = "alternate",
          mimeType = Some("text/html"),
          href = cIOnlineResource.linkage,
          title = cIOnlineResource.name))
      case Some(r"WWW:LINK-1.0-http--download(?:data)?") => {
        val mimeType = cIOnlineResource.linkage.getFile.toLowerCase match {
          case r".*\.jpe?g" => "image/jpeg"
          case r".*\.png" => "image/png"
          case r".*\.pdf" => "application/pdf"
          case r".*\.xlsx?" => "application/excel"
          case r".*\.txt" => "text/plain"
          case r".*\.csv" => "text/csv"
          case _ => "application/octet-stream"
        }
        Some(OwcLink(rel = "enclosure",
          mimeType = Some(mimeType),
          href = cIOnlineResource.linkage,
          title = cIOnlineResource.name))
      }
      case _ => cIOnlineResource.linkage match {
        case r"https?:\/\/data.linz.govt.nz\/layer\/.*" =>
          Some(OwcLink(rel = "alternates",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
        case r"https?:\/\/lris.scinfo.org.nz\/layer\/.*" =>
          Some(OwcLink(rel = "alternates",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
        case r"https?:\/\/geoportal\.doc\.govt\.nz\/(?i:ArcGIS)\/.*\/MapServer" =>
          Some(OwcLink(rel = "alternates",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
        case _ => cIOnlineResource.resourceType match {
          case ResourceType.WEBSITE => Some(OwcLink(rel = "alternate",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
          case _ => None
        }
      }
    }
  }

  /**
    * Converts OnlineResource to a List of OwcOffering. List might be empty, if the CIOnlineResource cannot be
    * converted to offerings in a sensible way.
    *
    * @return
    */
  def asOwcOfferings(cIOnlineResource: CIOnlineResource): List[OwcOffering] = {
    import utils.StringUtils._
    cIOnlineResource.linkage match {
      case r"https?:\/\/geoportal\.doc\.govt\.nz\/(?i:ArcGIS)\/.*\/MapServer" =>
        // GeoPortals ArcGIS server offers WMS/WFS for all layers I have seen. So we generate offerings for that.
        List(
          OwcOffering(code = WMS.code,
            operations = List(
              OwcOperation(
                code = "GetCapabilities",
                method = "GET",
                mimeType = Some("application/xml"),
                requestUrl = new URL(s"${cIOnlineResource.linkage}/WMSServer?request=GetCapabilities&service=WMS"))
            )
          ),
          OwcOffering(code = WFS.code,
            operations = List(
              OwcOperation(
                code = "GetCapabilities",
                method = "GET",
                mimeType = Some("application/xml"),
                requestUrl = new URL(s"${cIOnlineResource.linkage}/WFSServer?request=GetCapabilities&service=WFS"))
            )
          )
        )
      case _ => Nil
    }
  }

  /**
    * Converts MdMetadataSet to [[OwcResource]]
    *
    * @return
    */
  def asOwcResource(mdMetadataSet: MdMetadataSet, urlBaseForIds: String): OwcResource = {
    import utils.StringUtils._

    val offerings =
    // convert linkages to offerings (ist might be empty if there are no linkages that can be converted to offerings
      mdMetadataSet.linkage.flatMap(ci => asOwcOfferings(ci)) :::
        //Offering to get the original metadata document. Every document in the index should have that
        List(
          OwcOffering(code = CSW.code,
            operations = List(
              OwcOperation(
                code = "GetCapabilities",
                method = "GET",
                mimeType = Some("application/xml"),
                requestUrl = new URL(s"${mdMetadataSet.originUrl}?request=GetCapabilities&service=CSW")),
              OwcOperation(
                code = "GetRecordById",
                method = "GET",
                mimeType = Some("application/xml"),
                requestUrl = new URL(s"${mdMetadataSet.originUrl}?request=GetRecordById&version=2.0.2&service=CSW&elementSetName=full" +
                  "&outputSchema=http%3A%2F%2Fwww.isotc211.org%2F2005%2Fgmd" +
                  s"&Id=${URLEncoder.encode(mdMetadataSet.fileIdentifier, "UTF-8")}")))
          )
        )

    // ATTENTION: While id in OwcResource is of Type CharacterString, it still expects an IRI/URI type String, that'S why URL
    val baseUrl = new URL(urlBaseForIds)
    OwcResource(
      id = new URL(s"${baseUrl.getProtocol}://${baseUrl.getHost}/context/resource/${URLEncoder.encode(mdMetadataSet.fileIdentifier, "UTF-8")}"),
      geospatialExtent = Some(mdMetadataSet.bbox),
      title = mdMetadataSet.title,
      subtitle = mdMetadataSet.abstrakt.toOption(),
      updateDate = OffsetDateTime.of(mdMetadataSet.dateStamp.atStartOfDay(), ZoneId.systemDefault().getRules.getOffset(mdMetadataSet.dateStamp.atStartOfDay())),
      author = List(OwcAuthor(name = mdMetadataSet.contactName.toOption(), email = MdMetadataSet.parseEmailStringtoEmailAddress(mdMetadataSet.contactEmail), uri = None)),
      publisher = None,
      rights = mdMetadataSet.license.toOption(),
      temporalExtent = None,

      // links.alternates[] and rel=alternate
      contentDescription = mdMetadataSet.linkage.map(asOwcLink(_)).filter(_.isDefined)
        .map(_.get).filter(link => link.rel.equalsIgnoreCase("alternate")),

      // aka links.previews[] and rel=icon (atom)
      preview = mdMetadataSet.linkage.map(asOwcLink(_)).filter(_.isDefined)
        .map(_.get).filter(link => link.rel.equalsIgnoreCase("icon")),

      // aka links.data[] and rel=enclosure (atom)
      contentByRef = mdMetadataSet.linkage.map(asOwcLink(_)).filter(_.isDefined)
        .map(_.get).filter(link => link.rel.equalsIgnoreCase("enclosure")),

      // aka links.via[] & rel=via
      resourceMetadata = mdMetadataSet.linkage.map(asOwcLink(_)).filter(_.isDefined)
        .map(_.get).filter(link => link.rel.equalsIgnoreCase("via")),
      offering = offerings,
      minScaleDenominator = None,
      maxScaleDenominator = None,
      keyword = List(),
      folder = None)
  }

  /**
    * Convert List[MdMetadataSet] to OwcContext
    *
    * @param metadataList  List[MdMetadataSet] containing the entries
    * @param idAsUrlString String containing the ID for the OwcContext document
    */
  def toOwcContext(metadataList: List[MdMetadataSet], idAsUrlString: String): OwcContext = {
    val bbox = Some(GeoJSONFeatureCollectionWriter.getBoundingBoxAsRect(metadataList))

    val owcCreatorApplication = OwcCreatorApplication(
      title = Some(utils.BuildInfo.name),
      uri = Some(new URL(idAsUrlString)),
      version = Some(s"${utils.BuildInfo.version}-${utils.BuildInfo.buildNumber}")
    )

    implicit val localDateOrdering: Ordering[LocalDate] = Ordering.by(_.toEpochDay)
    val dateRange = metadataList.map(md => md.dateStamp).sorted
    val optDateInterval: Option[List[OffsetDateTime]] = if (dateRange.nonEmpty) {
      if (dateRange.tail.isEmpty) {
        Some(List(OffsetDateTime.of(dateRange.head, LocalTime.of(12, 0), ZoneOffset.UTC)))
      } else {
        Some(List(OffsetDateTime.of(dateRange.last, LocalTime.of(12, 0), ZoneOffset.UTC)))
      }
    } else {
      None
    }

    val owcResources = metadataList.map(md => asOwcResource(md, idAsUrlString))
    OwcContext(
      id = new URL(idAsUrlString),
      areaOfInterest = bbox,

      // aka links.profiles[] and rel=profile
      specReference = List(OwcProfile.CORE.newOf),

      // e.g. links.via[] and rel=via
      contextMetadata = List(),
      language = "en",
      title = "Search result list CSW ingester",
      subtitle = Some("Originating query: "),
      updateDate = OffsetDateTime.now(),
      author = List(),
      publisher = None,
      creatorApplication = Some(owcCreatorApplication),
      creatorDisplay = None,
      rights = None,
      timeIntervalOfInterest = optDateInterval,
      keyword = List(),
      resource = owcResources)
  }
}
