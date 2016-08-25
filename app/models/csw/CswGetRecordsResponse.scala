package models.csw

import java.time.format.DateTimeFormatter

import scala.xml.Elem

/**
  * Holds a GetRecordsResponse.
  *
  * @param xml
  */
class CswGetRecordsResponse(val xml: Elem) {
  //Parse XML
  val (numberOfRecordsReturned: Int, numberOfRecordsMatched: Int, nextRecord: Int) =
    xml.label match {
      case "GetRecordsResponse" => {
        (asInt(xml \ "SearchResults" \@ "numberOfRecordsReturned"),
          asInt(xml \ "SearchResults" \@ "numberOfRecordsMatched"),
          asInt(xml \ "SearchResults" \@ "nextRecord"))
      }
      case _ => throw new IllegalArgumentException(f"Expected Root Element <csw:GetRecordsResponse> but found ${xml.label}")
    }

  /**
    * converts string to int and assigns default value if exception.
    * @param s
    * @return
    */
  //FIXME SR this should be something like an "extension" to string. https://www.safaribooksonline.com/library/view/scala-cookbook/9781449340292/ch01s11.html
  private def asInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case e: NumberFormatException => 0
    }
  }

}

/**
  * CswGetRecordsResponse companion object
  */
object CswGetRecordsResponse {
  /**
    * creates CswGetRecordsResponse from XML
    *
    * @param xml
    * @return
    */
  def apply(xml: Elem): CswGetRecordsResponse = {
    new CswGetRecordsResponse(xml)
  }

  /**
    * creates empty CswGetRecordsResponse
    *
    * @return
    */
  def empty(): CswGetRecordsResponse = {
    CswGetRecordsResponse(
      <csw:GetRecordsResponse xsi:schemaLocation="http://www.opengis.net/cat/csw/2.0.2 http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd"
                              version="2.0.2" xmlns:sitemap="http://www.sitemaps.org/schemas/sitemap/0.9"
                              xmlns:csw="http://www.opengis.net/cat/csw/2.0.2">
        <csw:SearchStatus timestamp={java.time.LocalDateTime.now().format(DateTimeFormatter.ISO_INSTANT)}/>
        <csw:SearchResults elementSet="full" recordSchema="http://www.isotc211.org/2005/gmd"
                           numberOfRecordsReturned="0" numberOfRecordsMatched="0" nextRecord="0">
        </csw:SearchResults>
      </csw:GetRecordsResponse>
    )
  }

}
