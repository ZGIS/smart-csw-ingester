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
import utils.ClassnameLogger
import utils.StringUtils._

import scala.xml.NodeSeq

/**
  * Holds a subset of MDMetadata
  *
  * @param fileIdentifier the unique file identifier of the record
  * @param dateStamp      a datestamp of the record
  * @param title          title of the record
  * @param abstrakt       abstract of the record
  * @param keywords       list of keywords for the record
  * @param topicCategory  list of ISO/ANZLIC topic categories of the record
  * @param contactName    contact name for the record or dataset
  * @param contactOrg     cantact organisation for the record or dataset
  * @param contactEmail   contact email for the record or dataset
  * @param license        licenses for the record or dataset
  * @param bbox           bbox of the record
  * @param origin         origin i.e. source catalogue where the record was loaded from
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

  override def toString: String = {
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

  /**
    * returns the object as LuceneDocument
    *
    * @return LuceneDocument for index
    */
  def asLuceneDocument(): Document = {
    val doc = new Document()

    doc.add(new TextField("fileIdentifier", fileIdentifier, Field.Store.YES))
    doc.add(new TextField("title", title, Field.Store.YES))
    doc.add(new TextField("abstrakt", abstrakt, Field.Store.YES))
    doc.add(new TextField("dateStampText", dateStampAsIsoString, Field.Store.YES))

    val longDate = dateStamp.toEpochDay
    doc.add(new LongPoint("dateStamp", longDate))

    keywords.foreach(keyword => {
      doc.add(new TextField("keywords", keyword, Field.Store.YES))
    })

    topicCategory.foreach(topicCategor => {
      doc.add(new TextField("topicCategory", topicCategor, Field.Store.YES))
    })

    doc.add(new TextField("contactName", contactName, Field.Store.YES))
    doc.add(new TextField("contactOrg", contactOrg, Field.Store.YES))
    doc.add(new TextField("contactEmail", contactEmail, Field.Store.YES))
    doc.add(new TextField("license", license, Field.Store.YES))

    doc.add(new TextField("bboxText", bboxAsWkt, Field.Store.YES))

    val ctx = SpatialContext.GEO
    val bboxStrategy: BBoxStrategy = BBoxStrategy.newInstance(ctx, "bbox")
    val bboxFields = bboxStrategy.createIndexableFields(bbox)
    bboxFields.foreach(field => doc.add(field))

    doc.add(new StringField("origin", origin, Field.Store.YES))

    //FIXME decide if use catch_all field or how to build a query that queries all fields
    doc.add(new TextField("catch_all", fileIdentifier, Field.Store.YES))
    doc.add(new TextField("catch_all", title, Field.Store.YES))
    doc.add(new TextField("catch_all", abstrakt, Field.Store.YES))
    keywords.foreach(keyword => {
      doc.add(new TextField("catch_all", keyword, Field.Store.YES))
    })
    topicCategory.foreach(topicCategor => {
      doc.add(new TextField("catch_all", topicCategor, Field.Store.YES))
    })
    doc.add(new TextField("catch_all", contactName, Field.Store.YES))
    doc.add(new TextField("catch_all", contactOrg, Field.Store.YES))
    doc.add(new TextField("catch_all", contactEmail, Field.Store.YES))
    doc.add(new TextField("catch_all", license, Field.Store.YES))
    doc.add(new TextField("catch_all", origin, Field.Store.YES))

    doc
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

  /**
    * converts LuceneDocument into MdMetadataSet from index
    *
    * @param doc
    * @return MdMetadataSet extracted from retrieved LuceneDocument
    */
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
      logger.warn(f"Could not parse and of the dates (${dateStrings.mkString(", ")})")
      LocalDate.ofEpochDay(0) //1970-01-01
    }
    else {
      if (datesList.size > 1) {
        logger.warn(f"Could parse ${datesList.size} of (${dateStrings.mkString(", ")}) only returning first success")
      }
      datesList.head
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
    * extracts keywords fields from the provided XML
    *
    * @param nodeSeq the provided xml
    * @return a List(String) of extracted fields
    */
  def keywordsFromXml(nodeSeq: NodeSeq): List[String] = {
    (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "descriptiveKeywords" \ "MD_Keywords" \ "keyword" \ "CharacterString").map(
      elem => elem.text.trim).toList
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
      (nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "resourceConstraints" \ "MD_LegalConstraints" \ "useLimitation" \ "CharacterString").map(
        elem => elem.text.trim).mkString(", ")
    val metaConstraints = (nodeSeq \\ "metadataConstraints" \ "MD_LegalConstraints" \ "useLimitation" \ "CharacterString").map(
      elem => elem.text.trim).mkString(", ")
    List(resConstraints, metaConstraints).mkString(", ")
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
            logger.warn(f"changing longitude value $n to $result")
            result
          }
          case _ => {
            val result = math.signum(value) * minLon + (value % maxLon)
            logger.warn(f"changing longitude value $value to $result")
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
    logger.debug("PARSED RECT: " + rect.toString)
    rect
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
      bboxFromCoords(west, east, south, north)
    }
    else {
      logger.warn(f"${(nodeSeq \\ "fileIdentifier" \ "CharacterString").text} has no BBOX")
      bboxFromCoords(minLon, maxLon, minLat, maxLat)
    }
//    bboxFromCoords(165, 360 - 175, -53.1, -28.8)
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
        Json.arr(gmd.bbox.getMaxX+360, gmd.bbox.getMaxY), //NE
        Json.arr(gmd.bbox.getMaxX+360, gmd.bbox.getMinY), //NW
        Json.arr(gmd.bbox.getMinX, gmd.bbox.getMinY)  //SW
      ))
      Json.obj("type" -> "Polygon", "coordinates" -> newCoordinates)
    }
    else {
      Json.parse(geometry)
    }
  }

  /**
    * if bbox crosses dateline (east < west) then correct this for OL3
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
      "topicCategory" -> gmd.topicCategory,
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
      "origin" -> gmd.origin
    )

    Json.obj(
      "type" -> "Feature",
      "geometry" -> getJsGeom(gmd),
      "properties" -> properties
    )
  }
}

/**
  * GeoJSON FeatureCollection writer for List of [[MdMetadataSet]]
  *
  * @see [[Writes]]
  */
object GeoJSONFeatureCollectionWriter extends Writes[List[MdMetadataSet]] with ClassnameLogger {

  private lazy val ctx = SpatialContext.GEO
  private lazy val jsWriter = ctx.getFormats().getWriter(ShapeIO.GeoJSON)
  implicit val gmdElementSetWrite = MdMetadataSetWriter
  lazy val WORLD = ctx.getWorldBounds

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

    val envelope = if (shapeCollection.size > 0) {
      shapeCollection.getBoundingBox
    }
                   else {
                     WORLD
                   }

    Json.arr(
      JsNumber(envelope.getMinX),
      JsNumber(envelope.getMinY),
      JsNumber(envelope.getMaxX),
      JsNumber(envelope.getMaxY)
    )
  }

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
}

