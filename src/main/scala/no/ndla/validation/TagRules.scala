package no.ndla.validation

import org.json4s.native.JsonMethods._

object TagRules {
  case class TagAttributeRules(required: Set[TagAttributes.Value], optional: Seq[Set[TagAttributes.Value]], validSrcDomains: Option[Seq[String]]) {
    lazy val all: Set[TagAttributes.Value] = required ++ optional.flatten
  }

  def toTagAttributeRules(map: Map[String, Any]) = {
    val optionalAttrs: List[List[TagAttributes.Value]] = map.get("optional")
      .map(_.asInstanceOf[List[List[String]]].map(_.flatMap(TagAttributes.valueOf))).getOrElse(List.empty)
    val validSrcDomains: Option[Seq[String]] = map.get("validSrcDomains").map(_.asInstanceOf[Seq[String]])
    val requiredAttrs: List[TagAttributes.Value] = map.get("required")
      .map(_.asInstanceOf[List[String]].flatMap(TagAttributes.valueOf))
      .getOrElse(List.empty)

    TagAttributeRules(
      requiredAttrs.toSet,
      optionalAttrs.map(_.toSet),
      validSrcDomains
    )
  }

  def convertJsonStr(jsonStr: String): Map[String, Any] = {
    implicit val formats = org.json4s.DefaultFormats
    parse(jsonStr).extract[Map[String, Any]]
  }
}

object TagAttributes extends Enumeration {
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
  val DataOpenIn = Value("data-open-in")
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
  val Lang = Value("lang")

  def all: Set[String] = TagAttributes.values.map(_.toString)

  def valueOf(s: String): Option[TagAttributes.Value] = {
    TagAttributes.values.find(_.toString == s)
  }
}