package models

import java.time._
import java.time.format._
import java.util.UUID

import org.locationtech.spatial4j.context.SpatialContext
import org.locationtech.spatial4j.io.ShapeIO
import org.locationtech.spatial4j.shape.Rectangle

import scala.xml.NodeSeq

/**
  * from https://github.com/ZGIS/smart-csw-ingester/issues/4
  *
  *  file identifier / uuid
  *  latest datestamp
  *  keywords and keyword lists
  *  title
  *  abstract
  *  contact names and organisation
  *  license
  *  BBOX
  *  source / origon catalog url
  *
  * Created by kmoch on 22/08/2016.
  */
case class GmdElementSet (fileIdentifier: String,
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
                          origin: String) {

  def getWktBbox() : String = {
    val ctx = SpatialContext.GEO
    val shpWriter = ctx.getFormats().getWriter(ShapeIO.WKT)
    shpWriter.toString(bbox)
  }

  // TODO probably delete, might not always work?
  def getUuid() : java.util.UUID = {
    UUID.fromString(fileIdentifier)
  }

  def isoLocalDateText() : String = {
    dateStamp.format(DateTimeFormatter.ISO_LOCAL_DATE)
  }
}

object GmdElementSet {

  val ctx = SpatialContext.GEO
  val shpReader = ctx.getFormats().getReader(ShapeIO.WKT)

  def fromXml (nodeSeq: NodeSeq) : GmdElementSet = {
    fromXml (nodeSeq, "")
  }

  def fromXml (nodeSeq: NodeSeq, origin: String) : GmdElementSet = {
    GmdElementSet(
      ( nodeSeq \\ "fileIdentifier" \ "CharacterString" ).text,
      dateFromXml(nodeSeq),
      ( nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "citation" \ "CI_Citation" \ "title" \ "CharacterString" ).text,
      ( nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "abstract" \ "CharacterString" ).text,
      keywordsFromXml(nodeSeq),
      topicCategoriesFromXml(nodeSeq),
      contactNameFromXml(nodeSeq),
      contactOrgFromXml(nodeSeq),
      contactEmailFromXml(nodeSeq),
      licenseFromXml(nodeSeq),
      bboxFromXml(nodeSeq),
      origin
    )
  }

  // TODO check if ZoneInfo in date, alternatively either UTC or NZ TimeZone
  // check if Time info in date/datestamp
  // check decide dateStamp and/or/plus CI_Date date
  def dateFromXml (nodeSeq: NodeSeq) : LocalDate = {
    val dateString = ( nodeSeq \\"dateStamp" \ "Date" ).text
    val parsedDate = try {
      LocalDate.parse(dateString, DateTimeFormatter.ISO_LOCAL_DATE)
    } catch {
      case ex: DateTimeParseException => LocalDate.of(1970, Month.JANUARY, 1)
      // case e => _
    }
    parsedDate
  }

  def keywordsFromXml(nodeSeq: NodeSeq) : List[String] = {
    ( nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "descriptiveKeywords" \ "MD_Keywords" \ "keyword" \ "CharacterString" ).map( elem => elem.text).toList
  }

  def topicCategoriesFromXml(nodeSeq: NodeSeq) : List[String] = {
    ( nodeSeq \\ "identificationInfo" \ "MD_DataIdentification" \ "topicCategory" \ "MD_TopicCategoryCode" ).map( elem => elem.text).toList
  }

  def contactNameFromXml(nodeSeq: NodeSeq) : String = {
    ( nodeSeq \\ "CI_ResponsibleParty" \ "individualName" \ "CharacterString" ).map( elem => elem.text).mkString(", ")
  }

  def contactOrgFromXml(nodeSeq: NodeSeq) : String = {
    ( nodeSeq \\ "CI_ResponsibleParty" \ "organisationName" \ "CharacterString" ).map( elem => elem.text).mkString(", ")
  }
  def contactEmailFromXml(nodeSeq: NodeSeq) : String = {
    ( nodeSeq \\ "CI_ResponsibleParty" \ "contactInfo" \ "CI_Contact" \ "address" \ "CI_Address" \ "electronicMailAddress" \ "CharacterString" ).map( elem => elem.text).mkString(", ")
  }

  def licenseFromXml(nodeSeq: NodeSeq) : String = {
    val resConstraints = ( nodeSeq \\"identificationInfo" \ "MD_DataIdentification" \ "resourceConstraints" \ "MD_LegalConstraints" \ "useLimitation" \ "CharacterString" ).map( elem => elem.text).mkString(", ")
    val metaConstraints = ( nodeSeq \\"metadataConstraints" \ "MD_LegalConstraints" \ "useLimitation" \ "CharacterString" ).map( elem => elem.text).mkString(", ")
    List(resConstraints, metaConstraints).mkString(", ")
  }

  def bboxFromXml (nodeSeq: NodeSeq) : Rectangle = {

    val bboxXml = ( nodeSeq \\ "extent" \ "EX_Extent")
    val west = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "westBoundLongitude" \ "Decimal").text.toDouble
    val east = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "eastBoundLongitude" \ "Decimal").text.toDouble
    val north = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "northBoundLatitude" \ "Decimal").text.toDouble
    val south = (bboxXml \\ "geographicElement" \ "EX_GeographicBoundingBox" \ "southBoundLatitude" \ "Decimal").text.toDouble

    // Rectangle rect(double minX, double maxX, double minY, double maxY);
    ctx.getShapeFactory().rect(east, west, south, north )
  }


}


