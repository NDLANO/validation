/*
 * Part of NDLA validation.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.validation

import org.json4s.native.JsonMethods.parse
import org.json4s._
import scala.language.postfixOps
import scala.io.Source

object EmbedTagRules {
  val ResourceHtmlEmbedTag = "embed"

  case class EmbedThings(attrsForResource: Map[ResourceType.Value, EmbedTagAttributeRules])
  case class EmbedTagAttributeRules(required: Set[Attributes.Value], optional: Seq[Set[Attributes.Value]], validSrcDomains: Set[String]) {
    lazy val all: Set[Attributes.Value] = required ++ optional.flatten
  }

  private[validation] lazy val attributeRules: Map[ResourceType.Value, EmbedTagAttributeRules] = embedRulesToJson

  lazy val allEmbedTagAttributes: Set[Attributes.Value] = attributeRules.flatMap { case (_ , attrRules)  => attrRules.all } toSet

  def attributesForResourceType(resourceType: ResourceType.Value): EmbedTagAttributeRules = attributeRules(resourceType)

  private def embedRulesToJson = {
    val attrs = convertJsonStr(Source.fromResource("embed-tag-rules.json").mkString)
      .get("attrsForResource").map(_.asInstanceOf[Map[String, Map[String, Any]]])

    def toEmbedTagAttributeRules(map: Map[String, Any]) = {
      val optionalAttrs: List[List[Attributes.Value]] = map.get("optional")
        .map(_.asInstanceOf[List[List[String]]].map(_.flatMap(Attributes.valueOf))).getOrElse(List.empty)
      val validSrcDomains: Seq[String] = map.get("validSrcDomains").map(_.asInstanceOf[Seq[String]]).getOrElse(Seq.empty)

      EmbedTagAttributeRules(
        map("required").asInstanceOf[Seq[String]].flatMap(Attributes.valueOf).toSet,
        optionalAttrs.map(_.toSet),
        validSrcDomains.toSet
      )
    }

    def strToResourceType(str: String): ResourceType.Value =
      ResourceType.valueOf(str).getOrElse(throw new ConfigurationException(s"Missing declaration of resource type '$str' in ResourceType enum"))

    attrs.get.map {
      case (resourceType, attrRules) => strToResourceType(resourceType) -> toEmbedTagAttributeRules(attrRules)
    }
  }

  private def convertJsonStr(jsonStr: String): Map[String, Any] = {
    implicit val formats = org.json4s.DefaultFormats
    parse(jsonStr).extract[Map[String, Any]]
  }
}

object ResourceType extends Enumeration {
  val Error = Value("error")
  val Image = Value("image")
  val Audio = Value("audio")
  val H5P = Value("h5p")
  val Brightcove = Value("brightcove")
  val ContentLink = Value("content-link")
  val ExternalContent = Value("external")
  val NRKContent = Value("nrk")
  val ConceptLink = Value("concept")
  val Prezi = Value("prezi")
  val Commoncraft = Value("commoncraft")
  val NdlaFilmIundervisning = Value("ndla-filmiundervisning")
  val Kahoot = Value("kahoot")
  val KhanAcademy = Value("khan-academy")
  val FootNote = Value("footnote")
  val RelatedContent = Value("related-content")

  def all: Set[String] = ResourceType.values.map(_.toString)

  def valueOf(s: String): Option[ResourceType.Value] = {
    ResourceType.values.find(_.toString == s)
  }
}

object Attributes extends Enumeration {
  val DataUrl = Value("data-url")
  val DataAlt = Value("data-alt")
  val DataSize = Value("data-size")
  val DataAlign = Value("data-align")
  val DataWidth = Value("data-width")
  val DataHeight = Value("data-height")
  val DataPlayer = Value("data-player")
  val DataMessage = Value("data-message")
  val DataCaption = Value("data-caption")
  val DataAccount = Value("data-account")
  val DataVideoId = Value("data-videoid")
  val DataResource = Value("data-resource")
  val DataLinkText = Value("data-link-text")
  val DataContentId = Value("data-content-id")
  val DataNRKVideoId = Value("data-nrk-video-id")
  val DataResource_Id = Value("data-resource_id")
  val DataTitle = Value("data-title")
  val DataType = Value("data-type")
  val DataYear = Value("data-year")
  val DataEdition = Value("data-edition")
  val DataPublisher = Value("data-publisher")
  val DataAuthors = Value("data-authors")
  val DataArticleIds = Value("data-article-ids")

  val DataUpperLeftY =  Value("data-upper-left-y")
  val DataUpperLeftX = Value("data-upper-left-x")
  val DataLowerRightY = Value("data-lower-right-y")
  val DataLowerRightX = Value("data-lower-right-x")
  val DataFocalX = Value("data-focal-x")
  val DataFocalY = Value("data-focal-y")

  val XMLNsAttribute = Value("xmlns")

  val Href = Value("href")
  val Title = Value("title")
  val Align = Value("align")
  val Valign = Value("valign")
  val Target = Value("target")
  val Rel = Value("rel")

  def all: Set[String] = Attributes.values.map(_.toString)

  def valueOf(s: String): Option[Attributes.Value] = {
    Attributes.values.find(_.toString == s)
  }
}

