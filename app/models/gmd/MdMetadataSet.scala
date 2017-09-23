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

import java.time._
import java.time.format._
import java.util

import org.apache.lucene.document._
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io.ShapeIO
import org.locationtech.spatial4j.shape.{Rectangle, ShapeCollection}
import play.api.libs.json.{JsObject, _}
import uk.gov.hmrc.emailaddress.EmailAddress
import utils.ClassnameLogger
import utils.StringUtils._

import scala.util.{Success, Try}
import scala.xml.NodeSeq

/**
  * Holds a subset of MDMetadata
  *
  * @param fileIdentifier  the unique file identifier of the record
  * @param dateStamp       a datestamp of the record
  * @param title           title of the record
  * @param abstrakt        abstract of the record
  * @param keywords        list of keywords for the record
  * @param topicCategories list of ISO/ANZLIC topic categories of the record
  * @param contactName     contact name for the record or dataset
  * @param contactOrg      cantact organisation for the record or dataset
  * @param contactEmail    contact email for the record or dataset
  * @param license         licenses for the record or dataset
  * @param bbox            bbox of the record
  * @param lineageStmt     Lineage Statement
  * @param linkage         List of Linkages
  * @param origin          origin i.e. source catalogue where the record was loaded from
  */
case class MdMetadataSet(fileIdentifier: String,
                         dateStamp: LocalDate,
                         title: String,
                         abstrakt: String,
                         keywords: List[String],
                         smartCategory: List[String],
                         topicCategories: List[String],
                         contactName: String,
                         contactOrg: String,
                         contactEmail: String,
                         license: String,
                         bbox: Rectangle,
                         lineageStmt: String,
                         linkage: List[CIOnlineResource],
                         origin: String,
                         originUrl: String,
                         searchScore: Float = 0.0f) extends ClassnameLogger {
  require(!fileIdentifier.trim.isEmpty, "FileIdentifier was empty")

  override def toString: String = {
    f"""MdMetadataSet(
       |$fileIdentifier,
       |${dateStampAsIsoString},
       |${title},
       |${abstrakt},
       |keywords(${keywords.mkString(", ")}),
       |smartCategory(${smartCategory.mkString(", ")}),
       |topicCategory(${topicCategories.mkString(", ")}),
       |${contactName},
       |${contactOrg},
       |${contactEmail},
       |${license},
       |${bboxAsWkt},
       |${lineageStmt},
       |${linkage},
       |${origin},
       |${originUrl}
       |${searchScore}
       |)
     """.stripMargin.replaceAll("\n", " ")
  }

  /**
    * returns the object as LuceneDocument
    *
    * @return LuceneDocument for index
    */
  def asLuceneDocument: Document = {
    val doc = new Document()

    // don't use, only for index uniqueness and doc update
    doc.add(new StringField("id", fileIdentifier, Field.Store.YES))

    doc.add(new TextField("fileIdentifier", fileIdentifier, Field.Store.YES))
    doc.add(new TextField("title", title, Field.Store.YES))
    doc.add(new TextField("abstrakt", abstrakt, Field.Store.YES))
    doc.add(new TextField("dateStampText", dateStampAsIsoString, Field.Store.YES))

    val longDate = dateStamp.toEpochDay
    doc.add(new LongPoint("dateStamp", longDate))

    keywords.foreach(keyword => {
      doc.add(new TextField("keywords", keyword, Field.Store.YES))
    })

    smartCategory.foreach(smartCategory => {
      doc.add(new TextField("smartCategory", smartCategory, Field.Store.YES))
    })

    topicCategories.foreach(topicCategory => {
      doc.add(new TextField("topicCategory", topicCategory, Field.Store.YES))
    })

    doc.add(new TextField("contactName", contactName, Field.Store.YES))
    doc.add(new TextField("contactOrg", contactOrg, Field.Store.YES))
    doc.add(new TextField("contactEmail", contactEmail, Field.Store.YES))
    doc.add(new TextField("license", license, Field.Store.YES))
    doc.add(new TextField("bboxText", bboxAsWkt, Field.Store.YES))
    doc.add(new TextField("lineageStmt", lineageStmt, Field.Store.YES))
    linkage.foreach(linkage => {
      doc.add(new TextField("linkage", linkage.linkage.toString, Field.Store.YES))
      doc.add(new TextField("linkageFull", Json.toJson(linkage).toString(), Field.Store.YES))
    })

    val ctx = SpatialContext.GEO
    val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(ctx, "bbox")
    val bboxFields = bboxStrategy.createIndexableFields(bbox)
    bboxFields.foreach(field => doc.add(field))

    doc.add(new TextField("origin", origin, Field.Store.YES))
    doc.add(new TextField("originUrl", originUrl, Field.Store.YES))

    //FIXME decide if use catch_all field or how to build a query that queries all fields
    doc.add(new TextField("catch_all", fileIdentifier, Field.Store.YES))
    doc.add(new TextField("catch_all", title, Field.Store.YES))
    doc.add(new TextField("catch_all", abstrakt, Field.Store.YES))
    keywords.foreach(keyword => {
      doc.add(new TextField("catch_all", keyword, Field.Store.YES))
    })
    smartCategory.foreach(smartCategory => {
      doc.add(new TextField("catch_all", smartCategory, Field.Store.YES))
    })
    topicCategories.foreach(topicCategor => {
      doc.add(new TextField("catch_all", topicCategor, Field.Store.YES))
    })
    doc.add(new TextField("catch_all", contactName, Field.Store.YES))
    doc.add(new TextField("catch_all", contactOrg, Field.Store.YES))
    doc.add(new TextField("catch_all", contactEmail, Field.Store.YES))
    doc.add(new TextField("catch_all", license, Field.Store.YES))
    doc.add(new TextField("catch_all", lineageStmt, Field.Store.YES))
    doc.add(new TextField("catch_all", origin, Field.Store.YES))

    doc
  }

  /**
    * returns bounding box as well known text (WKT)
    *
    * @return Envelope (WKT) representation of BBOX
    */
  private def bboxAsWkt: String = {
    val ctx = SpatialContext.GEO
    val shpWriter = ctx.getFormats().getWriter(ShapeIO.WKT)
    shpWriter.toString(bbox)
  }

  /**
    * returns dateStamp as String in ISO_LOCAL_DATE format.
    *
    * @return ISO_LOCAL_DATE formatted date string
    */
  def dateStampAsIsoString: String = {
    dateStamp.format(DateTimeFormatter.ISO_LOCAL_DATE)
  }
}

/**
  * Companion Object to [[MdMetadataSet]]
  */
object MdMetadataSet extends ClassnameLogger {
  private lazy val ctx = SpatialContext.GEO
  private lazy val wktReader = ctx.getFormats().getReader(ShapeIO.WKT)
  private lazy val minLon = ctx.getWorldBounds.getMinX
  private lazy val maxLon = ctx.getWorldBounds.getMaxX
  private lazy val minLat = ctx.getWorldBounds.getMinY
  private lazy val maxLat = ctx.getWorldBounds.getMaxY

  /**
    * Creates [[MdMetadataSet]] from MD_Medatada XML
    *
    * @param nodeSeq XML containing MD_Metadata
    * @return Some(MdMetadataSet) or None if parsing error
    */
  def fromXml(nodeSeq: NodeSeq): Option[MdMetadataSet] = {
    fromXml(nodeSeq, "", "")
  }

  /**
    * Creates [[MdMetadataSet]] from MD_Medatada XML
    *
    * @param nodeSeq XML containing MD_Metadata
    * @param origin  catalogue name of origin
    * @return Some(MdMetadataSet) or None if parsing error
    */
  def fromXml(nodeSeq: NodeSeq, origin: String, originUrl: String): Option[MdMetadataSet] = {
    try {
      nodeSeq.head.label match {
        case "MD_Metadata" =>
          Some(MdMetadataSet((nodeSeq \ "fileIdentifier" \ "CharacterString").text, dateFromXml(nodeSeq), (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "citation" \ "CI_Citation" \ "title" \ "CharacterString").text, (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "abstract" \ "CharacterString").text, keywordsFromXml(nodeSeq), smartCategoryFromXml(nodeSeq), topicCategoriesFromXml(nodeSeq), contactNameFromXml(nodeSeq), contactOrgFromXml(nodeSeq), contactEmailFromXml(nodeSeq), licenseFromXml(nodeSeq), bboxFromXml(nodeSeq), lineageFromXml(nodeSeq), linkageFromXml(nodeSeq, origin), origin, originUrl))
        case _ =>
          throw new IllegalArgumentException(f"Expected MDMetadataNode but found  ${nodeSeq.head.label}")
      }
    }
    catch {
      //FIXME SR replace by specific exceptions
      case e: Exception => logger.warn(f"Exception on parsing MD_Metadata: ${e.getMessage}", e)
        None
    }
  }

  /**
    * extracts a LocalDate from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a LoclDate from the list of extracted fields
    */
  def dateFromXml(nodeSeq: NodeSeq): LocalDate = {
    val dateNodes = (nodeSeq \\ "dateStamp" \ "Date") ++
      (nodeSeq \\ "dateStamp" \ "DateTime") ++
      (nodeSeq \\ "date" \ "CI_Date" \ "date" \ "Date") ++
      (nodeSeq \\ "date" \ "CI_Date" \ "date" \ "DateTime")
    dateFromStrings(dateNodes.map(node => node.text).toList)
  }

  /**
    * check if Time info in date/datestamp
    * check decide dateStamp and/or/plus CI_Date date
    * worst case create default date
    *
    * TODO AK ponder if precise ZoneInfo for parsedDate needed, alternatively either UTC or NZ TimeZone
    *
    * @param dateStrings
    * @return
    */
  def dateFromStrings(dateStrings: List[String]): LocalDate = {
    val yearMonthMatcher = """^\d\d\d\d-\d\d$""".r
    val yearMatcher = """^\d\d\d\d$""".r
    val isoInstantMatcher = """.*Z$""".r

    val acceptedDateFormats = List(
      DateTimeFormatter.ISO_LOCAL_DATE_TIME, //2011-12-03T10:15:30
      DateTimeFormatter.ISO_LOCAL_DATE, //2011-12-03
      DateTimeFormatter.BASIC_ISO_DATE, //20111203
      DateTimeFormatter.ISO_OFFSET_DATE, //2011-12-03+01:00
      DateTimeFormatter.ISO_OFFSET_DATE_TIME //2011-12-03T10:15:30+01:00
      //TODO SR DateTimeFormatter.ISO_INSTANT cannot be parsed into LocalDate but only LocalDateTime?
      // DateTimeFormatter.ISO_INSTANT //2011-12-03T10:15:30Z
    )

    val datesList = dateStrings.filter(!_.trim.isEmpty).flatMap(//iterate over parameter list
      dateString => acceptedDateFormats.map(//try all parsers per date
        df => {
          try {
            val correctedDateString = dateString match {
              case yearMonthMatcher() => dateString.concat("-01")
              case yearMatcher() => dateString.concat("-01-01")
              case isoInstantMatcher() => dateString.dropRight(1)
              case _ => dateString
            }
            Some(LocalDate.parse(correctedDateString, df))
          }
          catch {
            case e: DateTimeParseException => None
          }
        }
      ).filter(_.isDefined)).map(_.get) //filter None and remove the Option

    if (datesList.isEmpty) {
      logger.warn(f"Could not parse any of the dates (${dateStrings.mkString(", ")})")
      LocalDate.ofEpochDay(0) //1970-01-01
    }
    else {
      if (datesList.size > 1) {
        logger.info(f"Could parse ${datesList.size} of (${dateStrings.mkString(", ")}) only returning first success")
      }
      datesList.head
    }
  }

  /**
    * extracts keywords fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a List(String) of extracted fields
    */
  def keywordsFromXml(nodeSeq: NodeSeq): List[String] = {
    val kwNode = (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "descriptiveKeywords").filter(p => {
      //TODO SR internally for smart csw we use "theme" as codelistValue. What are we going to do about that here?
      !((p \ "MD_Keywords" \ "type" \ "MD_KeywordTypeCode" \ "@codeListValue").text.equals("SMART"))
    })

    val resultList = (kwNode \ "MD_Keywords" \ "keyword" \ "CharacterString").map(elem => elem.text.trim).toList
    logger.debug(s"found keywords: $resultList")
    resultList
  }

  /**
    * extracts smartCategory fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a List(String) of extracted fields
    */
  def smartCategoryFromXml(nodeSeq: NodeSeq): List[String] = {
    val kwNode = (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "descriptiveKeywords").filter(p => {
      ((p \ "MD_Keywords" \ "type" \ "MD_KeywordTypeCode" \ "@codeListValue").text.equals("SMART"))
    })
    val resultList = (kwNode \ "MD_Keywords" \ "keyword" \ "CharacterString").map(elem => elem.text.trim).toList
    logger.debug(s"found smartCategory: $resultList")
    resultList
  }

  /**
    * extracts topicCategory fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a List(String) of extracted fields
    */
  def topicCategoriesFromXml(nodeSeq: NodeSeq): List[String] = {
    (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "topicCategory" \ "MD_TopicCategoryCode").map(
      elem => elem.text).toList
  }

  /**
    * extracts contactName fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a string concatenation of extracted fields
    */
  def contactNameFromXml(nodeSeq: NodeSeq): String = {
    (nodeSeq \\ "CI_ResponsibleParty" \ "individualName" \ "CharacterString").map(elem => elem.text).mkString(", ").trim
  }

  /**
    * extracts contact Organisation fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a string concatenation of extracted fields
    */
  def contactOrgFromXml(nodeSeq: NodeSeq): String = {
    (nodeSeq \\ "CI_ResponsibleParty" \ "organisationName" \ "CharacterString").map(elem => elem.text).mkString(
      ", ").trim
  }

  /**
    * extracts contact email fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a string concatenation of extracted fields
    */
  def contactEmailFromXml(nodeSeq: NodeSeq): String = {
    (nodeSeq \\ "CI_ResponsibleParty" \ "contactInfo" \ "CI_Contact" \ "address" \ "CI_Address" \ "electronicMailAddress" \ "CharacterString").map(
      elem => elem.text.trim).mkString(", ")
  }

  /**
    * extracts some license fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a string concatenation of extracted fields
    */
  def licenseFromXml(nodeSeq: NodeSeq): String = {
    val resConstraints =
      (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "resourceConstraints" \ "MD_LegalConstraints"
        \ "useLimitation" \ "CharacterString")
        .map(elem => elem.text.trim).mkString(", ")
    val metaConstraints =
      (nodeSeq \\ "metadataConstraints" \ "MD_LegalConstraints" \ "useLimitation" \ "CharacterString")
        .map(elem => elem.text.trim).mkString(", ")

    List(resConstraints, metaConstraints).mkString(", ")
  }

  /**
    * extracts lineage statement from XML
    *
    * @param nodeSeq the provided XML
    * @return String with the lineage statement
    */
  def lineageFromXml(nodeSeq: NodeSeq): String = {
    (nodeSeq \\ "MD_Metadata" \\ "dataQualityInfo" \\ "DQ_DataQuality" \\ "lineage" \\ "LI_Lineage").map(
      elem => {
        logger.debug(s"Lineage statement '${(elem \\ "statement" \\ "CharacterString").text.trim}'")
        (elem \\ "statement" \\ "CharacterString").text.trim
      }
    ).mkString(", ")
  }

  /**
    * Extracts linkage from XML. Looks for CI_OnlineResource in MD_Distribution
    *
    * @param nodeSeq the provided XML
    * @param origin  name of the catalogue
    * @return
    */
  def linkageFromXml(nodeSeq: NodeSeq, origin: String): List[CIOnlineResource] = {
    // technically there can be linkages in CI_Contact, but that is more like the website of the contact and not linkages to the data itself
    // gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:onlineResource
    // gmd:MD_Metadata/gmd:identificationInfo/srv:SV_ServiceIdentification/srv:containsOperations/srv:SV_OperationMetadata/srv:connectPoint/gmd:CI_OnlineResource
    // gmd:MD_Metadata/gmd:metadataExtensionInfo/gmd:MD_MetadataExtensionInformation/gmd:extensionOnlineResource
    (
      // gmd:MD_Metadata/gmd:distributionInfo/gmd:MD_Distribution/gmd:transferOptions/gmd:MD_DigitalTransferOptions/gmd:onLine
      (nodeSeq \\ "MD_Metadata" \\ "distributionInfo" \\ "MD_Distribution" \\ "transferOptions" \\
        "MD_DigitalTransferOptions" \\ "onLine" \\ "CI_OnlineResource") ++
        // gmd:MD_Metadata/gmd:distributionInfo/gmd:MD_Distribution/gmd:distributor/gmd:MD_Distributor/gmd:distributorTransferOptions/gmd:MD_DigitalTransferOptions/gmd:online
        (nodeSeq \\ "MD_Metadata" \\ "distributionInfo" \\ "MD_Distribution" \\ "distributor" \\ "MD_Distributor" \\
          "distributorTransferOptions" \\ "MD_DigitalTransferOptions" \\ "onLine" \\ "CI_OnlineResource")
      )
      .map(elem => CIOnlineResource.fromXml(elem, origin)
      ).toList
  }

  /**
    * tries to extract a valid and usable bounding box rectangle from xml
    *
    * @param nodeSeq xml nodeseq containing an extent element
    * @return [[Rectangle]] the bounding box
    */
  def bboxFromXml(nodeSeq: NodeSeq): Rectangle = {
    val bboxXml = (nodeSeq \\ "extent" \ "EX_Extent")
    logger.trace(f"bboxFromXml: ${bboxXml}")
    if (bboxXml.size == 1) {
      val westText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "westBoundLongitude" \ "Decimal").text
      val eastText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "eastBoundLongitude" \ "Decimal").text
      val southText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "southBoundLatitude" \ "Decimal").text
      val northText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "northBoundLatitude" \ "Decimal").text
      val west = westText.toDoubleWithDefault(minLon)
      val east = eastText.toDoubleWithDefault(maxLon)
      val south = southText.toDoubleWithDefault(minLat)
      val north = northText.toDoubleWithDefault(maxLat)
      // Rectangle rect(double minX, double maxX, double minY, double maxY);
      logger.debug(s"parsed bbox (${west}, ${east}, ${south}, ${north})")
      bboxFromCoords(west, east, south, north)
    }
    else {
      logger.warn(f"${(nodeSeq \\ "fileIdentifier" \ "CharacterString").text} has no BBOX")
      bboxFromCoords(minLon, maxLon, minLat, maxLat)
    }
    //    bboxFromCoords(165, 360 - 175, -53.1, -28.8)
  }

  /**
    * tries to build a bounding box rectangle as safely as possible from provided coordinates
    * Rectangle rect(double minX, double maxX, double minY, double maxY);
    * bboxFromCoords(west, east, south, north)
    *
    * @param west  most western value / minX
    * @param east  most eastern value / maxX
    * @param south most southern value / minY
    * @param north most northern value / maxY
    * @return the resulting bounding box
    */
  def bboxFromCoords(west: Double, east: Double, south: Double, north: Double): Rectangle = {
    val (prunedWest, prunedEast) = pruneLongitudeValues(west, east)
    val (prunedSouth, prunedNorth) = pruneLatitudeValues(south, north)

    val rect = ctx.getShapeFactory().rect(prunedWest, prunedEast, prunedSouth, prunedNorth)
    logger.debug(s"parsed rect ${rect.toString}")
    rect
  }

  /**
    * tries to naively prune the provided coordinates into good shape for WSG84
    * TODO DATE Line Wraps :-( ?
    * Rectangle rect(double minX, double maxX, double minY, double maxY);
    * bboxFromCoords(west, east, south, north)
    *
    * @param west most western value / minY
    * @param east most eastern value / maxY
    * @return tuple of viable coordinates in WSG84
    */
  def pruneLongitudeValues(west: Double, east: Double): (Double, Double) = {
    if (math.abs(west - east) > math.abs(minLon - maxLon)) {
      (minLon, maxLon) //in case the rectangle spans more than 360 deg make it world
    }
    else {
      val result = List(west, east).map({ (value: Double) =>
        value match {
          case n if value >= minLon && value <= maxLon => n
          case n if math.abs(value % math.abs(minLon - maxLon)) < maxLon => {
            val result = value % maxLon
            logger.info(f"changing longitude value $n to $result")
            result
          }
          case _ => {
            val result = math.signum(value) * minLon + (value % maxLon)
            logger.info(f"changing longitude value $value to $result")
            result
          }
        }
      })
      (result(0), result(1))
    }
  }

  /**
    * Cuts off latitudes outside of minLax / maxLat and swaps if south > north
    *
    * @param south most southern value / minY
    * @param north most northern value / maxY
    * @return tuple of viable coordinates
    */
  def pruneLatitudeValues(south: Double, north: Double): (Double, Double) = {
    //min/max in tuples swaps north/south if necessary,
    (Math.max(minLat, Math.min(south, north)),
      Math.min(maxLat, Math.max(south, north)))
  }

  /**
    * converts LuceneDocument into MdMetadataSet from index
    *
    * @param doc [[Document]] containing the MdMetadataSet
    * @return [[MdMetadataSet]] extracted from retrieved LuceneDocument
    */
  def fromLuceneDoc(doc: Document, score: Float): MdMetadataSet = {
    MdMetadataSet(fileIdentifier = doc.get("fileIdentifier"), dateStamp = MdMetadataSet.dateFromStrings(List(doc.get("dateStampText"))), title = doc.get("title"), abstrakt = doc.get("abstrakt"), keywords = doc.getValues("keywords").toList, smartCategory = doc.getValues("smartCategory").toList, topicCategories = doc.getValues("topicCategory").toList, contactName = doc.get("contactName"), contactOrg = doc.get("contactOrg"), contactEmail = doc.get("contactEmail"), license = doc.get("license"), bbox = MdMetadataSet.bboxFromWkt(doc.get("bboxText")), lineageStmt = doc.get("lineageStmt"), linkage = doc.getValues("linkageFull").toList.map(str =>
            Json.fromJson[CIOnlineResource](Json.parse(str)).get
          ), origin = doc.get("origin"), originUrl = doc.get("originUrl"), score)
  }

  /**
    * returns an Option[EmailAddress] object if parsing is successful
    *
    * @param emailString the string that might comprise of or contain an email address
    * @return Option[EmailAddress]
    */
  def parseEmailStringtoEmailAddress(emailString: String): Option[EmailAddress] = {

    if (EmailAddress.isValid(emailString)) {
      Some(EmailAddress(emailString))
    } else {
      if (emailString.contains(",")) {
        val commaFree = emailString.split(",").head.trim

        Try {
          EmailAddress(commaFree)
        } match {
          case Success(e) => Some(e)
          case _ => None
        }
      } else {
        None
      }
    }
  }

  /**
    * naively feeds a WKT string to the Spatial4J Shapereader to transform into a Rectangle
    *
    * @param envelope the WKT envelope as String
    * @return [[Rectangle]] the bounding box as object
    */
  //TODO SR move to StringUtils?
  def bboxFromWkt(envelope: String): Rectangle = {
    // https://github.com/locationtech/spatial4j/blob/master/FORMATS.md beware, typo?
    // Rectangle ENVELOPE(1, 2, 4, 3) (minX, maxX, maxY, minY)
    // https://github.com/locationtech/spatial4j/blob/master/src/main/java/org/locationtech/spatial4j/io/WKTReader.java#L245
    wktReader.read(envelope).asInstanceOf[Rectangle]
  }
}


/**
  * GeoJSON writer for [[MdMetadataSet]]
  *
  * @see [[Writes]]
  */
object MdMetadataSetWriter extends Writes[MdMetadataSet] with ClassnameLogger {

  private lazy val ctx = SpatialContext.GEO
  private lazy val jsWriter = ctx.getFormats().getWriter(ShapeIO.GeoJSON)

  /**
    * Converts [[MdMetadataSet]] object into [[JsObject]] as GeoJSON
    */
  def writes(gmd: MdMetadataSet): JsObject = {
    val properties = Json.obj(
      "fileIdentifier" -> gmd.fileIdentifier,
      "dateStamp" -> gmd.dateStampAsIsoString,
      "title" -> gmd.title,
      "abstrakt" -> gmd.abstrakt,
      "keywords" -> gmd.keywords,
      "smartCategory" -> gmd.smartCategory,
      "topicCategories" -> gmd.topicCategories,
      "contactName" -> gmd.contactName,
      "contactOrg" -> gmd.contactOrg,
      "contactEmail" -> gmd.contactEmail,
      "license" -> gmd.license,
      // extent is an array [10, 10, 40, 40] minX, maxX, maxY, minY
      "bbox" -> correctBbox(Json.arr(
        JsNumber(gmd.bbox.getMinX()),
        JsNumber(gmd.bbox.getMinY()),
        JsNumber(gmd.bbox.getMaxX()),
        JsNumber(gmd.bbox.getMaxY()))
      ),
      "lineageStmt" -> gmd.lineageStmt,
      "linkage" -> gmd.linkage.map(Json.toJson(_)),
      "origin" -> gmd.origin,
      "originUrl" -> gmd.originUrl,
      "searchScore" -> gmd.searchScore
    )

    Json.obj(
      "type" -> "Feature",
      "geometry" -> getJsGeom(gmd),
      "properties" -> properties
    )
  }

  /**
    * provides the GeoJSON Polygon geometry from the feature's Rectangle bbox
    *
    * @param gmd MdMetadataSet
    * @return JsValue with GeoJSON Polygon geometry
    */
  def getJsGeom(gmd: MdMetadataSet): JsValue = {
    val geometry = jsWriter.toString(gmd.bbox)

    if (gmd.bbox.getCrossesDateLine) {
      val newCoordinates = Json.arr(Json.arr(
        Json.arr(gmd.bbox.getMinX, gmd.bbox.getMinY), //SW
        Json.arr(gmd.bbox.getMinX, gmd.bbox.getMaxY), //NW
        Json.arr(gmd.bbox.getMaxX + 360, gmd.bbox.getMaxY), //NE
        Json.arr(gmd.bbox.getMaxX + 360, gmd.bbox.getMinY), //NW
        Json.arr(gmd.bbox.getMinX, gmd.bbox.getMinY) //SW
      ))
      Json.obj("type" -> "Polygon", "coordinates" -> newCoordinates)
    }
    else {
      Json.parse(geometry)
    }
  }

  /**
    * if bbox crosses dateline (east < west) then correct this for OL3
    *
    * @param bbox
    * @return
    */
  def correctBbox(bbox: JsArray): JsArray = {
    val west = bbox(0).asOpt[Double].get
    val east = if (bbox(0).asOpt[Double].get > bbox(2).asOpt[Double].get) {
      bbox(2).asOpt[Double].get + 360
    }
    else {
      bbox(2).asOpt[Double].get
    }
    Json.arr(west, bbox(1).get, east, bbox(3).get)
  }
}

/**
  * GeoJSON FeatureCollection writer for List of [[MdMetadataSet]]
  *
  * @see [[Writes]]
  */
object GeoJSONFeatureCollectionWriter extends Writes[List[MdMetadataSet]] with ClassnameLogger {
  lazy val WORLD = ctx.getWorldBounds
  //  private lazy val jsWriter = ctx.getFormats().getWriter(ShapeIO.GeoJSON)
  implicit val gmdElementSetWrite = MdMetadataSetWriter
  private lazy val ctx = SpatialContext.GEO

  /**
    * Converts List of [[MdMetadataSet]] object into [[JsObject]] as GeoJSON
    */
  def writes(gmdList: List[MdMetadataSet]): JsObject = {
    if (gmdList.size > 0) {
      Json.obj("type" -> "FeatureCollection",
        "crs" -> Json.obj(
          "type" -> "name",
          "properties" -> Json.obj(
            "name" -> "urn:ogc:def:crs:OGC:1.3:CRS84"
          )
        ),
        "bbox" -> MdMetadataSetWriter.correctBbox(getBoundingBox(gmdList).as[JsArray]),
        "count" -> gmdList.size,
        "features" -> getArrayOfFeatures(gmdList)
      )
    }
    else {
      Json.obj("type" -> "FeatureCollection",
        "crs" -> Json.obj(
          "type" -> "name",
          "properties" -> Json.obj(
            "name" -> "urn:ogc:def:crs:OGC:1.3:CRS84"
          )
        ),
        "count" -> 0,
        "features" -> Json.arr()
      )
    }
  }

  /**
    * builds a JsArray from single MdMetadataSet GeoJSON features
    *
    * @param gmdList List [[MdMetadataSet]]
    * @return JsArray MdMetadataSet
    */
  def getArrayOfFeatures(gmdList: List[MdMetadataSet]): JsValue = {
    val jsList = gmdList.map(gmd => {
      Json.toJson(gmd)
    })
    Json.toJson(jsList)
  }

  /**
    * computes the overall bounding box for the featurecollection
    *
    * @param gmdList List [[MdMetadataSet]]
    * @return JsArray with 4 bbox double values (e, w, s, n)
    */
  def getBoundingBox(gmdList: List[MdMetadataSet]): JsValue = {
    val envelope: Rectangle = getBoundingBoxAsRect(gmdList)
    Json.arr(
      JsNumber(envelope.getMinX),
      JsNumber(envelope.getMinY),
      JsNumber(envelope.getMaxX),
      JsNumber(envelope.getMaxY)
    )
  }

  def getBoundingBoxAsRect(gmdList: List[MdMetadataSet]): Rectangle = {
    import collection.JavaConverters._
    import collection.mutable._

    logger.debug(f"gmdList.size() ${gmdList.size}")

    val shapeBuffer: scala.collection.mutable.Buffer[Rectangle] = Buffer[Rectangle]()
    gmdList.foreach(gmd =>
      shapeBuffer.append(gmd.bbox)
    )
    logger.debug(f"shapeBuffer.size() ${shapeBuffer.size}")

    val shapeList: java.util.List[Rectangle] = new util.ArrayList[Rectangle]
    shapeList.addAll(shapeBuffer.asJava)
    logger.debug(f"shapeList.size() ${shapeList.size}")
    // shapes - Copied by reference! (make a defensive copy if caller modifies), also needs RandomAccess
    // https://locationtech.github.io/spatial4j/apidocs/org/locationtech/spatial4j/shape/ShapeCollection.html
    val shapeCollection = new ShapeCollection[Rectangle](shapeList, ctx)
    logger.debug(f"shapeCollection.size() ${shapeCollection.size}")

    if (shapeCollection.size > 0) {
      shapeCollection.getBoundingBox
    }
    else {
      WORLD
    }
  }
}
