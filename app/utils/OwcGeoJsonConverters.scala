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

package utils

import java.net.{URL, URLEncoder}
import java.time._

import info.smart.models.owc100.OwcOfferingType._
import info.smart.models.owc100._
import models.gmd.{CIOnlineResource, GeoJSONFeatureCollectionWriter, MdMetadataSet, ResourceType}
import play.api.libs.MimeTypes

import scala.util.Try

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

    val filename = Try(cIOnlineResource.linkage.getFile.toLowerCase).toOption
    val mimetypeOption = filename.flatMap(fn => MimeTypes.forFileName(fn))

    cIOnlineResource.protocol match {
      case Some("WWW:LINK-1.0-http--metadata-URL") =>
        Some(OwcLink(rel = "via",
          mimeType = mimetypeOption.orElse(Some("application/xml")),
          href = cIOnlineResource.linkage,
          title = cIOnlineResource.name))
      case Some("WWW:LINK-1.0-http--link") =>
        Some(OwcLink(rel = "alternate",
          mimeType = mimetypeOption.orElse(Some("text/html")),
          href = cIOnlineResource.linkage,
          title = cIOnlineResource.name))
      case Some(r"WWW:LINK-1.0-http--download(?:data)?") =>
        Some(OwcLink(rel = "enclosure",
          mimeType = mimetypeOption.orElse(Some("application/octet-stream")),
          href = cIOnlineResource.linkage,
          title = cIOnlineResource.name))
      case Some("WWW:LINK-1.0-http--image-thumbnail") =>
        Some(OwcLink(rel = "icon",
          mimeType = mimetypeOption.orElse(Some("image/png")),
          href = cIOnlineResource.linkage,
          title = cIOnlineResource.name))

      case _ => cIOnlineResource.linkage.toString match {
        case r"https?:\/\/data.linz.govt.nz\/layer\/.*" =>
          Some(OwcLink(rel = "via",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
        case r"https?:\/\/lris.scinfo.org.nz\/layer\/.*" =>
          Some(OwcLink(rel = "via",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
        case r"https?:\/\/data.mfe.govt.nz\/layer\/.*" =>
          Some(OwcLink(rel = "via",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
        case r"https?:\/\/geoportal\.doc\.govt\.nz\/(?i:ArcGIS)\/.*\/MapServer" =>
          Some(OwcLink(rel = "via",
            mimeType = Some("text/html"),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
        case _ => cIOnlineResource.resourceType match {
          case ResourceType.WEBSITE => Some(OwcLink(rel = "alternate",
            mimeType = mimetypeOption.orElse(Some("text/html")),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
          case ResourceType.DOWNLOAD => Some(OwcLink(rel = "enclosure",
            mimeType = mimetypeOption.orElse(Some("application/octet-stream")),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
          case ResourceType.MAP => Some(OwcLink(rel = "alternate",
            mimeType = mimetypeOption.orElse(Some("text/html")),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
          case ResourceType.DATA => Some(OwcLink(rel = "enclosure",
            mimeType = mimetypeOption.orElse(Some("application/octet-stream")),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
          case ResourceType.METADATA => Some(OwcLink(rel = "via",
            mimeType = mimetypeOption.orElse(Some("text/xml")),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
          case ResourceType.IMAGE =>
            Some(OwcLink(rel = "icon",
              mimeType = mimetypeOption.orElse(Some("image/png")),
              href = cIOnlineResource.linkage,
              title = cIOnlineResource.name))
          case _ => Some(OwcLink(rel = "alternate",
            mimeType = mimetypeOption.orElse(None),
            href = cIOnlineResource.linkage,
            title = cIOnlineResource.name))
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

    cIOnlineResource.resourceType match {

      case ResourceType.SERVICE =>

        cIOnlineResource.protocol match {

          case Some(str) if str.contains("OGC:") =>
            generateOwcOfferingFromServiceProto(str, cIOnlineResource.linkage)

          case _ => Nil
        }
      case ResourceType.METADATA =>

        cIOnlineResource.linkage.toString match {

          case r"https?:\/\/geoportal\.doc\.govt\.nz\/(?i:ArcGIS)\/.*\/MapServer" =>
            generateOwcOfferingForArcServer(cIOnlineResource.linkage)
          case _ => Nil
        }
      case ResourceType.MAP =>

        // TODO, some of those are available through WMS or WFS access, but require API key
        // also not known how to find out from MD XML (not specified)
        cIOnlineResource.linkage.toString match {
          case r"https?:\/\/data.linz.govt.nz\/layer\/.*" => Nil
          case r"https?:\/\/data.mfe.govt.nz\/layer\/.*" => Nil
          case r"https?:\/\/lris.scinfo.org.nz\/layer\/.*" => Nil
        }
      case _ => Nil
    }
  }

  /**
    * Converts List[String] to List of [[OwcCategory]]
    *
    * @param md
    * @return
    */
  def asOwcCategory(md: MdMetadataSet): List[OwcCategory] = {
    val hLevel = OwcCategory(term = md.hierarchyLevel, scheme = Some("hierarchyLevel"), label = Some(md.hierarchyLevel))
    List(hLevel) ++
      md.keywords.map(k => OwcCategory(term = k, scheme = None, label = Some(k))) ++
      md.smartCategory.map(k => OwcCategory(term = k, scheme = Some("SAC Categories"), label = Some(k)))
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
      publisher = mdMetadataSet.contactOrg.toOption(),
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
      keyword = asOwcCategory(mdMetadataSet),
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
      subtitle = Some("Originating from query"),
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

  /**
    * tries to guess SERVICE based OGC offerings
    *
    * @param protocolString
    * @param url
    * @return
    */
  def generateOwcOfferingFromServiceProto(protocolString: String, url: URL): List[OwcOffering] = {

    // OGC:WCS-1.0.0-http-get-capabilities
    // OGC:WCS-1.0.0-http-get-coverage
    // OGC:WMS-1.3.0-http-get-map
    // OGC:WFS-2.0.0-http-get-feature
    // OGC:SOS-1.0.0-http-get-observation
    val protoInfo = protocolString.split(":")
    if (protoInfo.length > 1) {
      if (protoInfo(0).equalsIgnoreCase("OGC")) {
        val operationInfo = protoInfo(1).split("-")
        val offeringCode = operationInfo(0)
        val offeringType = Try(applyOfferingType(offeringCode)).toOption
        if (operationInfo.length > 1) {
          val version = operationInfo(1)
          val http = operationInfo(2)
          val part1 = operationInfo(3)
          val part2 = operationInfo(4)
          val operationName = part1.toLowerCase.capitalize + part2.toLowerCase.capitalize
          offeringType.map {
            ot =>
              OwcOffering(code = ot.code,
                operations = List(
                  OwcOperation(
                    code = operationName,
                    method = "GET",
                    mimeType = Some("application/xml"),
                    requestUrl = new URL(s"${url.toString}?service=${offeringCode.toUpperCase}&request=$operationName&version=$version"))
                )
              )
          }.toList
        } else {

          offeringType.map {
            ot =>
              OwcOffering(code = ot.code,
                operations = List(
                  OwcOperation(
                    code = "GetCapabilities",
                    method = "GET",
                    mimeType = Some("application/xml"),
                    requestUrl = new URL(s"${url.toString}?request=GetCapabilities&service=${offeringCode.toUpperCase}"))
                )
              )
          }.toList
        }
      } else {
        List()
      }
    } else {
      List()
    }
  }

  /**
    * little helper to generate OwcOfferingType
    *
    * @param v
    * @return
    */
  def applyOfferingType(v: String): OwcOfferingType = {
    v.toUpperCase match {
      case "WMS" => WMS
      case "WFS" => WFS
      case "WCS" => WCS
      case "SOS" => SOS
      case "WPS" => WPS
      case "CSW" => CSW
      case _ =>
        logger.warn(s"Value $v is not a supported OwcOfferingType value")
        throw new IllegalArgumentException(s"Value $v is not a supported OwcOfferingType value")
    }
  }

  def generateOwcOfferingForArcServer(url: URL): List[OwcOffering] = {

    // GeoPortals ArcGIS server offers WMS/WFS for all layers I have seen. So we generate offerings for that.
    // from http://geoportal.doc.govt.nz/ArcGIS/rest/services/GeoportalServices/DOC_BDIPEST_HimalayanThar_2007/MapServer
    // to http://geoportal.doc.govt.nz/arcgis/services/GeoportalServices/DOC_Operations_Regions/MapServer/WFSServer?request=GetCapabilities&service=WFS

    val urlParts = url.getPath.split("/")
    logger.debug(urlParts.length.toString)

    if (urlParts.length == 7) {
      logger.debug(urlParts.mkString("::"))
      if (urlParts(0).isEmpty &&
        urlParts(1).equalsIgnoreCase("ArcGIS") &&
        urlParts(2).equalsIgnoreCase("rest") &&
        urlParts(3).equalsIgnoreCase("services")
      ) {
        logger.debug(s"${url.getProtocol}://${url.getHost}/arcgis/services/${urlParts(4)}/${urlParts(5)}/${urlParts(6)}")
        val owcBaseUrl = new URL(s"${url.getProtocol}://${url.getHost}/arcgis/services/${urlParts(4)}/${urlParts(5)}/${urlParts(6)}")
        List(
          OwcOffering(code = WMS.code,
            operations = List(
              OwcOperation(
                code = "GetCapabilities",
                method = "GET",
                mimeType = Some("application/xml"),
                requestUrl = new URL(s"${owcBaseUrl.toString}/WMSServer?request=GetCapabilities&service=WMS"))
            )
          ),
          OwcOffering(code = WFS.code,
            operations = List(
              OwcOperation(
                code = "GetCapabilities",
                method = "GET",
                mimeType = Some("application/xml"),
                requestUrl = new URL(s"${owcBaseUrl.toString}/WFSServer?request=GetCapabilities&service=WFS"))
            )
          )
        )
      } else {
        List()
      }
    } else {
      List()
    }
  }
}
