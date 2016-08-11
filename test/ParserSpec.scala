
import org.scalatestplus.play._
import play.api.inject.guice.GuiceApplicationBuilder
import scala.xml.XML

class ParserSpec extends PlaySpec with OneAppPerSuite {

  // Override app if you need a Application with other than
  // default parameters.
  // val application: Application = GuiceApplicationBuilder().build()

  implicit override lazy val app = new GuiceApplicationBuilder().configure(Map("ehcacheplugin" -> "disabled")).build()

  "The OneAppPerSuite trait" must {

    "provide an Application" in {
      app.configuration.getString("ehcacheplugin") mustBe Some("disabled")
    }

    "Load XML Test Resource" in {
      val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
      val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)
      xml1.toString() must include ("Land Information New Zealand")

      val asResource2 = this.getClass().getResource("sac_rs_ewt_metadata.xml")
      val xml2: scala.xml.NodeSeq = scala.xml.XML.load(asResource2)
      xml2.text.contains("Hydrogeology") mustEqual(true)

    }

    "Evaluate Basic Xpath type queries" in {
      val asResource1 = this.getClass().getResource("csw_getrecordbyid-md_metadata.xml")
      val xml1: scala.xml.NodeSeq = scala.xml.XML.load(asResource1)

      (xml1 \\ "GetRecordByIdResponse" \ "MD_Metadata" \ "fileIdentifier" \ "CharacterString").text mustBe "23bdd7a3-fd21-daf1-7825-0d3bdc256f9d"

      (xml1 \\ "identificationInfo" \ "MD_DataIdentification" \ "citation" \ "CI_Citation" \ "title" \ "CharacterString" ).text mustBe "NZ Primary Road Parcels"

      // gmd:language/gco:CharacterString

      // gmd:characterSet/gmd:MD_CharacterSetCode

      // gmd:hierarchyLevel/gmd:MD_ScopeCode
      (xml1 \\ "hierarchyLevel" \ "MD_ScopeCode" ).text mustBe "dataset"

      // gmd:hierarchyLevelName/gco:CharacterString

      // this/gmd:contact/gmd:CI_ResponsibleParty/gmd:individualName/gco:CharacterString

      // gmd:contact/gmd:CI_ResponsibleParty/gmd:organisationName/gco:CharacterString

      // gmd:contact/gmd:CI_ResponsibleParty/gmd:organisationName/gco:CharacterString

      // gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:phone/gmd:CI_Telephone/gmd:voice/gco:CharacterString

      // gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:phone/gmd:CI_Telephone/gmd:facsimile/gco:CharacterString

      // gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:deliveryPoint/gco:CharacterString"

      // this/gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:city/gco:CharacterString");

      // /gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:postalCode/gco:CharacterString"

      // gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:country/gco:CharacterString");

      //gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:onlineResource/gmd:CI_OnlineResource/gmd:linkage/gmd:URL");
      (xml1 \\ "CI_OnlineResource" \ "linkage" \ "URL" ).text mustBe "https://data.linz.govt.nz/layer/796-nz-primary-road-parcels/"

      // his/gmd:contact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:electronicMailAddress/gco:CharacterString

      //gmd:contact/gmd:CI_ResponsibleParty/gmd:role/gmd:CI_RoleCode/codeList="./resources/codeList.xml#CI_RoleCode" codeListValue="pointOfContact"
      (xml1 \\ "contact" \ "CI_ResponsibleParty" \ "role" \ "CI_RoleCode" ).text mustBe "resourceProvider"

      // gmd:dateStamp/gco:DateTime"
      // (xml1 \\ "dateStamp" \ "DateTime")
      (xml1 \\ "dateStamp" \ "Date").text mustBe "2012-12-20"

      // gmd:metadataStandardName/gco:CharacterString "ISO 19115:2003/19139"

      // gmd:metadataStandardVersion/gco:CharacterString "1.0"

      // gmd:referenceSystemInfo/gmd:MD_ReferenceSystem/gmd:referenceSystemIdentifier/gmd:RS_Identifier/gmd:code/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:title/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:date/gmd:CI_Date/gmd:date/gco:DateTime

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:date/gmd:CI_Date/gmd:dateType/gmd:CI_DateTypeCode/codeList="./resources/codeList.xml#CI_DateTypeCode" codeListValue="revision

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:abstract/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:status/gmd:MD_ProgressCode/codeList="./resources/codeList.xml#MD_ProgressCode" codeListValue="completed">completed
      (xml1 \\ "identificationInfo" \ "MD_DataIdentification" \ "status" \ "MD_ProgressCode" ).text mustBe "onGoing"

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:purpose/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:individualName/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:organisationName/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/positionName/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:phone/gmd:CI_Telephone/gmd:voice/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:phone/gmd:CI_Telephone/gmd:facsimile/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:deliveryPoint/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:city/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:postalCode/gco:CharacterString

      // gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointofcontact/gmd:CI_ResponsibleParty/gmd:contactInfo/gmd:CI_Contact/gmd:address/gmd:CI_Address/gmd:country/gco:CharacterString

    }
  }
}

