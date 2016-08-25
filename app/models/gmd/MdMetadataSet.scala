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

package models.gmd

import java.time._
import java.time.format._

import org.apache.lucene.document.{Document, Field, LongPoint, TextField}
import org.apache.lucene.spatial.bbox.BBoxStrategy
import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io.ShapeIO
import org.locationtech.spatial4j.shape.Rectangle
import play.api.libs.json._
import utils.ClassnameLogger

import scala.xml.NodeSeq

/**
  * Holds a subset of MDMetadata
  *
  * @param fileIdentifier
  * @param dateStamp
  * @param title
  * @param abstrakt
  * @param keywords
  * @param topicCategory
  * @param contactName
  * @param contactOrg
  * @param contactEmail
  * @param license
  * @param bbox
  * @param origin
  */
case class MdMetadataSet(fileIdentifier: String,
                         dateStamp: LocalDate,
                         title: String,
                         abstrakt: String,
                         keywords: List[String],
                         topicCategory: List[String],
                         contactName: String,
                         contactOrg: String,
                         contactEmail: String,
                         license: String,
                         bbox: Rectangle,
                         origin: String) extends ClassnameLogger {
  require(!fileIdentifier.trim.isEmpty, "FileIdentifier was empty")

  override def toString = {
    f"""MdMetadataSet(
        |$fileIdentifier,
        |${dateStampAsIsoString},
        |${title},
        |${abstrakt},
        |keywords(${keywords.mkString(", ")}),
        |topicCategory(${topicCategory.mkString(", ")}),
        |${contactName},
        |${contactOrg},
        |${contactEmail},
        |${license},
        |${bboxAsWkt},
        |${origin})
     """.stripMargin.replaceAll("\n", " ")
  }

  /**
    * returns bounding box as well known text (WKT)
    *
    * @return
    */
  private def bboxAsWkt: String = {
    val ctx = SpatialContext.GEO
    val shpWriter = ctx.getFormats().getWriter(ShapeIO.WKT)
    shpWriter.toString(bbox)
  }

  /**
    * returns dateStamp as String in ISO_LOCAL_DATE format.
    *
    * @return
    */
  def dateStampAsIsoString: String = {
    dateStamp.format(DateTimeFormatter.ISO_LOCAL_DATE)
  }

  /**
    * returns the object as LuceneDocument
    *
    * @return
    */
  def asLuceneDocument(): Document = {
    val ctx = SpatialContext.GEO
    val doc = new Document()

    val longDate = dateStamp.toEpochDay
    val dateField = new LongPoint("dateStampCompare", 1)
    dateField.setLongValue(longDate)
    doc.add(dateField)

    val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(ctx, "bboxStrategy")
    val bboxFields = bboxStrategy.createIndexableFields(bbox)
    bboxFields.foreach(field => doc.add(field))

    doc.add(new Field("fileIdentifier", fileIdentifier, TextField.TYPE_STORED))
    doc.add(new Field("title", title, TextField.TYPE_STORED))
    doc.add(new Field("abstrakt", abstrakt, TextField.TYPE_STORED))
    // Range Query for Date as Long value, this field is to recreate the date object
    doc.add(new Field("dateStampText", dateStampAsIsoString, TextField.TYPE_STORED))
    doc.add(new Field("keywords", keywords.mkString(" "), TextField.TYPE_STORED))
    doc.add(new Field("topicCategory", topicCategory.mkString(" "), TextField.TYPE_STORED))
    doc.add(new Field("contactName", contactName, TextField.TYPE_STORED))
    doc.add(new Field("contactOrg", contactOrg, TextField.TYPE_STORED))
    doc.add(new Field("contactEmail", contactEmail, TextField.TYPE_STORED))
    doc.add(new Field("license", license, TextField.TYPE_STORED))
    // Bbox Query on spatial index, this textfield is to recreate the geometry
    doc.add(new Field("bboxText", bboxAsWkt, TextField.TYPE_STORED))
    doc.add(new Field("origin", origin, TextField.TYPE_STORED))

    //FIXME decide if use catch_all field or how to build a query that queries all fields
    doc.add(new Field("catch_all", fileIdentifier, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", title, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", abstrakt, TextField.TYPE_STORED))
    // Range Query for Date as Long value, this field is to recreate the date object
    doc.add(new Field("catch_all", dateStampAsIsoString, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", keywords.mkString(" "), TextField.TYPE_STORED))
    doc.add(new Field("catch_all", topicCategory.mkString(" "), TextField.TYPE_STORED))
    doc.add(new Field("catch_all", contactName, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", contactOrg, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", contactEmail, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", license, TextField.TYPE_STORED))
    // Bbox Query on spatial index, this textfield is to recreate the geometry
    doc.add(new Field("catch_all", bboxAsWkt, TextField.TYPE_STORED))
    doc.add(new Field("catch_all", origin, TextField.TYPE_STORED))

    doc
  }
}

/**
  * Companion Object to [[MdMetadataSet]]
  */
object MdMetadataSet extends ClassnameLogger {
  private lazy val ctx = SpatialContext.GEO
  private lazy val shpReader = ctx.getFormats().getReader(ShapeIO.WKT)

  /**
    * Creates [[MdMetadataSet]] from MD_Medatada XML
    *
    * @param nodeSeq XML containing MD_Metadata
    * @return Some(MdMetadataSet) or None if parsing error
    */
  def fromXml(nodeSeq: NodeSeq): Option[MdMetadataSet] = {
    fromXml(nodeSeq, "")
  }

  /**
    * Creates [[MdMetadataSet]] from MD_Medatada XML
    *
    * @param nodeSeq XML containing MD_Metadata
    * @param origin  catalogue name of origin
    * @return Some(MdMetadataSet) or None if parsing error
    */
  def fromXml(nodeSeq: NodeSeq, origin: String): Option[MdMetadataSet] = {
    try {
      nodeSeq.head.label match {
        case "MD_Metadata" =>
          Some(MdMetadataSet(
            (nodeSeq \ "fileIdentifier" \ "CharacterString").text,
            dateFromXml(nodeSeq),
            (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "citation" \ "CI_Citation" \ "title" \ "CharacterString").text,
            (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "abstract" \ "CharacterString").text,
            keywordsFromXml(nodeSeq),
            topicCategoriesFromXml(nodeSeq),
            contactNameFromXml(nodeSeq),
            contactOrgFromXml(nodeSeq),
            contactEmailFromXml(nodeSeq),
            licenseFromXml(nodeSeq),
            bboxFromXml(nodeSeq),
            origin
          ))
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

  def fromLuceneDoc(doc: Document): MdMetadataSet = {
    MdMetadataSet(
      fileIdentifier = doc.get("fileIdentifier"),
      dateStamp = MdMetadataSet.dateFromStrings(List(doc.get("dateStampText"))),
      title = doc.get("title"),
      abstrakt = doc.get("abstrakt"),
      keywords = doc.get("keywords").split(",").toList,
      topicCategory = doc.get("topicCategory").split(",").toList,
      contactName = doc.get("contactName"),
      contactOrg = doc.get("contactOrg"),
      contactEmail = doc.get("contactEmail"),
      license = doc.get("license"),
      bbox = MdMetadataSet.bboxFromWkt(doc.get("bboxText")),
      origin = doc.get("origin")
    )
  }

  // TODO check if ZoneInfo for parsedDate needed, alternatively either UTC or NZ TimeZone
  // check if Time info in date/datestamp
  // check decide dateStamp and/or/plus CI_Date date
  // worst case create default date, suggestions?
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
      //      DateTimeFormatter.ISO_INSTANT //2011-12-03T10:15:30Z - this cannot be parsed into LocalDate but only LocalDateTime?
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
      logger.warn(f"Could not parse and of the dates (${dateStrings.mkString(", ")})")
      LocalDate.ofEpochDay(0) //1970-01-01
    }
    else {
      if (datesList.size > 1)
        logger.warn(f"Could parse ${datesList.size} of (${dateStrings.mkString(", ")}) only returning first success")
      datesList.head
    }

    /*
        val datesList = dateStrings.filter(dateString => !dateString.isEmpty).map {
          dateString =>
            val offsetMatcher = new scala.util.matching.Regex("""\+\d\d:\d\d""", "offset")
            val noOffsetDateString = offsetMatcher.replaceFirstIn(dateString, "")

            val parsedDate: Option[LocalDate] = try {

              noOffsetDateString match {
                case str if str.contains("T") => {
                  if (str.contains("Z")) {
                    Some(LocalDate.parse(str.replace("Z", ""), DateTimeFormatter.ISO_LOCAL_DATE_TIME))
                  }
                  else {
                    Some(LocalDate.parse(str, DateTimeFormatter.ISO_LOCAL_DATE_TIME))
                  }
                }

                case _ =>
                  Some(LocalDate.parse(noOffsetDateString, DateTimeFormatter.ISO_LOCAL_DATE))
              }
            } catch {
              case ex: DateTimeParseException => {
                val yearMonthDayMatcher = new Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""", "year", "month", "day")
                val yearMonthDayMatcherTight = new Regex("""(\d\d\d\d)(\d\d)(\d\d)""", "year", "month", "day")
                val yearMonthMatcher = new Regex("""(\d\d\d\d)-(\d\d)""", "year", "month")
                val yearMatcher = new Regex("""(\d\d\d\d)""", "year")

                noOffsetDateString match {
                  case yearMonthDayMatcher(year, month, day) =>
                    Some(LocalDate.of(year.toInt, Month.of(month.toInt), day.toInt))
                  case yearMonthDayMatcherTight(year, month, day) =>
                    Some(LocalDate.of(year.toInt, Month.of(month.toInt), day.toInt))
                  case yearMonthMatcher(year, month) =>
                    Some(LocalDate.of(year.toInt, Month.of(month.toInt), 1))
                  case yearMatcher(year) =>
                    Some(LocalDate.of(year.toInt, Month.JANUARY, 1))
                  case _ => {
                    logger.warn(f"Bad date $dateString $noOffsetDateString")
                    None
                  }
                }
              }
              case e: Throwable => None
            }
            parsedDate
        }.filter(dateOpt => dateOpt.isDefined)

        // finally, either there is something or epoch 0 worst case
        if (datesList.isEmpty)
          LocalDate.of(1970, Month.JANUARY, 1)
        else
          datesList.head.getOrElse(LocalDate.of(1970, Month.JANUARY, 1))
    */
  }

  def dateFromXml(nodeSeq: NodeSeq): LocalDate = {
    val dateNodes = (nodeSeq \\ "dateStamp" \ "Date") ++
      (nodeSeq \\ "dateStamp" \ "DateTime") ++
      (nodeSeq \\ "date" \ "CI_Date" \ "date" \ "Date") ++
      (nodeSeq \\ "date" \ "CI_Date" \ "date" \ "DateTime")
    dateFromStrings(dateNodes.map(node => node.text).toList)
  }

  def keywordsFromXml(nodeSeq: NodeSeq): List[String] = {
    (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "descriptiveKeywords" \ "MD_Keywords" \ "keyword" \ "CharacterString").map(
      elem => elem.text.trim).toList
  }

  def topicCategoriesFromXml(nodeSeq: NodeSeq): List[String] = {
    (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "topicCategory" \ "MD_TopicCategoryCode").map(
      elem => elem.text).toList
  }

  def contactNameFromXml(nodeSeq: NodeSeq): String = {
    (nodeSeq \\ "CI_ResponsibleParty" \ "individualName" \ "CharacterString").map(elem => elem.text).mkString(", ").trim
  }

  def contactOrgFromXml(nodeSeq: NodeSeq): String = {
    (nodeSeq \\ "CI_ResponsibleParty" \ "organisationName" \ "CharacterString").map(elem => elem.text).mkString(
      ", ").trim
  }

  def contactEmailFromXml(nodeSeq: NodeSeq): String = {
    (nodeSeq \\ "CI_ResponsibleParty" \ "contactInfo" \ "CI_Contact" \ "address" \ "CI_Address" \ "electronicMailAddress" \ "CharacterString").map(
      elem => elem.text.trim).mkString(", ")
  }

  def licenseFromXml(nodeSeq: NodeSeq): String = {
    val resConstraints = (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "resourceConstraints" \ "MD_LegalConstraints" \ "useLimitation" \ "CharacterString").map(
      elem => elem.text.trim).mkString(", ")
    val metaConstraints = (nodeSeq \\ "metadataConstraints" \ "MD_LegalConstraints" \ "useLimitation" \ "CharacterString").map(
      elem => elem.text.trim).mkString(", ")
    List(resConstraints, metaConstraints).mkString(", ")
  }

  def bboxFromCoords(east: Double, west: Double, south: Double, north: Double): Rectangle = {
    // TODO DATE Line Wraps :-(
    if (east < -180) logger.warn(f"e $east < -180")
    if (west > 180) logger.warn(f"w $west > 180")
    if (west < east) logger.warn(f"w $west < e $east")

    if (south < -90) logger.warn(f"s $south > -90")
    if (north > 90) logger.warn(f"n $north > 90")
    if (north < south) logger.warn(f"n $north < s $south")

    ctx.getShapeFactory().rect(east, west, south, north)
  }

  def bboxFromWkt(envelope: String): Rectangle = {
    // https://github.com/locationtech/spatial4j/blob/master/FORMATS.md beware, typo?
    // Rectangle	ENVELOPE(1, 2, 4, 3) (minX, maxX, maxY, minY)
    // https://github.com/locationtech/spatial4j/blob/master/src/main/java/org/locationtech/spatial4j/io/WKTReader.java#L245
    shpReader.read(envelope).asInstanceOf[Rectangle]
  }

  //FIXME SR this is basically a "Double.from(String, default)". We should think about creating a project wide helper for this. Somewhere I had a "String.toInt()" already, that needs the same treatment.
  def extractLatLonNumber(textIn: String, default: Double): Double = {
    //    if (textIn.isEmpty) { //SR this should end in a NFE and thus is handled below
    //      default
    //    }
    //    else {
    val parsedDouble = try {
      Some(textIn.toDouble)
    }
    catch {
      case e: java.lang.NumberFormatException => {
        logger.warn(f"Bad lat/lon value: $textIn", e)
        Some(default)
      }
      case e: Exception => {
        //SR we should only catch specific exceptions
        logger.error(e.getLocalizedMessage, e.getCause)
        None
      }
    }
    parsedDouble.getOrElse(default)
    //    }
  }

  def bboxFromXml(nodeSeq: NodeSeq): Rectangle = {
    val bboxXml = (nodeSeq \\ "extent" \ "EX_Extent")
    logger.trace(f"bboxFromXml: ${bboxXml}")
    if (bboxXml.size == 1) {
      val eastText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "eastBoundLongitude" \ "Decimal").text
      val westText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "westBoundLongitude" \ "Decimal").text
      val southText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "southBoundLatitude" \ "Decimal").text
      val northText = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "northBoundLatitude" \ "Decimal").text
      val east = extractLatLonNumber(eastText, -180)
      val west = extractLatLonNumber(westText, 180)
      val south = extractLatLonNumber(southText, -90)
      val north = extractLatLonNumber(northText, 90)
      // Rectangle rect(double minX, double maxX, double minY, double maxY);
      bboxFromCoords(east, west, south, north)
    }
    else {
      logger.warn(f"${(nodeSeq \\ "fileIdentifier" \ "CharacterString").text} has no BBOX")
      bboxFromCoords(-180, 180, -90, 90)
    }
  }
}

/**
  * Json writer for [[MdMetadataSet]]
  *
  * @see [[Writes]]
  */
object MdMetadataSetWriter extends Writes[MdMetadataSet] {
  /**
    * Converts [[MdMetadataSet]] object into [[JsValue]]
    */
  def writes(gmd: MdMetadataSet) = Json.obj(
    "fileIdentifier" -> gmd.fileIdentifier,
    "dateStamp" -> gmd.dateStampAsIsoString,
    "title" -> gmd.title,
    "abstrakt" -> gmd.abstrakt,
    "keywords" -> gmd.keywords,
    "topicCategory" -> gmd.topicCategory,
    "contactName" -> gmd.contactName,
    "contactOrg" -> gmd.contactOrg,
    "contactEmail" -> gmd.contactEmail,
    "license" -> gmd.license,
    // extent is an array [10, 10, 40, 40] minX, maxX, maxY, minY
    "bbox" -> Json.arr(
      JsNumber(gmd.bbox.getMinX()),
      JsNumber(gmd.bbox.getMaxX()),
      JsNumber(gmd.bbox.getMaxY()),
      JsNumber(gmd.bbox.getMinY())
    ),
    "origin" -> gmd.origin
  )
}
