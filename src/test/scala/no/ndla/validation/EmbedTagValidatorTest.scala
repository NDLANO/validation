/*
 * Part of NDLA validation.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.validation

import no.ndla.mapping.UnitSuite
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag

class EmbedTagValidatorTest extends UnitSuite {
  val embedTagValidator = new EmbedTagValidator()

  private def generateAttributes(attrs: Map[String, String]): String = {
    attrs.toList.sortBy(_._1.toString).map { case (key, value) => s"""$key="$value"""" }.mkString(" ")
  }

  private def generateTagWithAttrs(attrs: Map[TagAttributes.Value, String]): String = {
    val strAttrs = attrs map{ case (k, v) => k.toString -> v }
    s"""<$ResourceHtmlEmbedTag ${generateAttributes(strAttrs)} />"""
  }

  private def findErrorByMessage(validationMessages: Seq[ValidationMessage], partialMessage: String) =
    validationMessages.find(_.message.contains(partialMessage))

  test("validate should return an empty sequence if input is not an embed tag") {
    embedTagValidator.validate("content", "<h1>hello</h1>") should equal (Seq())
  }

  test("validate should return validation error if embed tag uses illegal attributes") {
    val attrs = generateAttributes(Map(
      TagAttributes.DataResource.toString -> ResourceType.ExternalContent.toString,
      TagAttributes.DataUrl.toString -> "google.com", "illegalattr" -> "test"))

    val res = embedTagValidator.validate("content", s"""<$ResourceHtmlEmbedTag $attrs />""")
    findErrorByMessage(res, "illegal attribute(s) 'illegalattr'").size should be (1)
  }

  test("validate should return validation error if an attribute contains HTML") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.ExternalContent.toString,
      TagAttributes.DataUrl -> "<i>google.com</i>"))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, "contains attributes with HTML: data-url").size should be (1)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=image") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Image.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.Image} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if image embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Image.toString,
      TagAttributes.DataResource_Id -> "1234",
      TagAttributes.DataSize -> "fullbredde",
      TagAttributes.DataAlt -> "alternative text",
      TagAttributes.DataCaption -> "here is a rabbit",
      TagAttributes.DataAlign -> "left"
    ))
    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=audio") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Audio.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.Audio} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if audio embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Audio.toString,
      TagAttributes.DataResource_Id -> "1234"
    ))
    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=h5p") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.H5P.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.H5P} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if h5p embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.H5P.toString,
      TagAttributes.DataUrl -> "http://ndla.no/h5p/embed/1234"
    ))
    val t = embedTagValidator.validate("content", tag)
    t.size should be (0)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=brightcove") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Brightcove.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.Brightcove} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if brightcove embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Brightcove.toString,
      TagAttributes.DataCaption -> "here is a video",
      TagAttributes.DataVideoId -> "1234",
      TagAttributes.DataAccount -> "2183716",
      TagAttributes.DataPlayer -> "B28fas"))
    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=content-link") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.ContentLink.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.ContentLink} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if content-link embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.ContentLink.toString,
      TagAttributes.DataContentId -> "54",
      TagAttributes.DataLinkText -> "interesting article",
      TagAttributes.DataOpenIn -> "new-context"))
    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=error") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Error.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.Error} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if error embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Error.toString,
      TagAttributes.DataMessage -> "interesting article"))
    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=external") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.ExternalContent.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.ExternalContent} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if external embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.ExternalContent.toString,
      TagAttributes.DataUrl -> "https://www.youtube.com/watch?v=pCZeVTMEsik"))
    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should return validation error if embed tag does not contain required attributes for data-resource=nrk") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.NRKContent.toString))
    val res = embedTagValidator.validate("content", tag)
    findErrorByMessage(res, s"data-resource=${ResourceType.NRKContent} must contain the following attributes:").size should be (1)
  }

  test("validate should return no validation errors if nrk embed-tag is used correctly") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.NRKContent.toString,
      TagAttributes.DataNRKVideoId -> "123",
      TagAttributes.DataUrl -> "http://nrk.no/video/123"
    ))
    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should fail if only one optional attribute is specified") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Image.toString,
      TagAttributes.DataAlt-> "123",
      TagAttributes.DataCaption-> "123",
      TagAttributes.DataResource_Id-> "123",
      TagAttributes.DataSize-> "full",
      TagAttributes.DataAlign-> "left",
      TagAttributes.DataUpperLeftX-> "0",
      TagAttributes.DataFocalX -> "0"
    ))
    embedTagValidator.validate("content", tag).size should be (2)
  }

  test("validate should succeed if all optional attributes are specified") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Image.toString,
      TagAttributes.DataAlt-> "123",
      TagAttributes.DataCaption-> "123",
      TagAttributes.DataResource_Id-> "123",
      TagAttributes.DataSize-> "full",
      TagAttributes.DataAlign-> "left",
      TagAttributes.DataUpperLeftX-> "0",
      TagAttributes.DataUpperLeftY-> "0",
      TagAttributes.DataLowerRightX -> "1",
      TagAttributes.DataLowerRightY -> "1",
      TagAttributes.DataFocalX -> "0",
      TagAttributes.DataFocalY -> "1"
    ))

    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should succeed if all attributes in an attribute group are specified") {
    val tagWithFocal = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Image.toString,
      TagAttributes.DataAlt-> "123",
      TagAttributes.DataCaption-> "123",
      TagAttributes.DataResource_Id-> "123",
      TagAttributes.DataSize-> "full",
      TagAttributes.DataAlign-> "left",
      TagAttributes.DataFocalX -> "0",
      TagAttributes.DataFocalY -> "1"
    ))

    embedTagValidator.validate("content", tagWithFocal).size should be (0)


    val tagWithCrop = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Image.toString,
      TagAttributes.DataAlt-> "123",
      TagAttributes.DataCaption-> "123",
      TagAttributes.DataResource_Id-> "123",
      TagAttributes.DataSize-> "full",
      TagAttributes.DataAlign-> "left",
      TagAttributes.DataUpperLeftX-> "0",
      TagAttributes.DataUpperLeftY-> "0",
      TagAttributes.DataLowerRightX -> "1",
      TagAttributes.DataLowerRightY -> "1"
    ))

    embedTagValidator.validate("content", tagWithCrop).size should be (0)
  }

  test("validate should succeed if source url is from a legal domain") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Prezi.toString,
      TagAttributes.DataUrl -> "https://prezi.com",
      TagAttributes.DataWidth -> "1",
      TagAttributes.DataHeight -> "1"
    ))

    embedTagValidator.validate("content", tag).size should be (0)
  }

  test("validate should fail if source url is from an illlegal domain") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.Prezi.toString,
      TagAttributes.DataUrl -> "https://evilprezi.com",
      TagAttributes.DataWidth -> "1",
      TagAttributes.DataHeight -> "1"
    ))

    val result = embedTagValidator.validate("content", tag)
    result.size should be (1)
    result.head.message.contains(s"can only contain ${TagAttributes.DataUrl} urls from the following domains:")  should be(true)
  }

  test("validate should succeed if source url is from a legal wildcard domain") {
    val tag = generateTagWithAttrs(Map(
      TagAttributes.DataResource -> ResourceType.KhanAcademy.toString,
      TagAttributes.DataUrl -> "https://thisisatest.khanacademy.org",
      TagAttributes.DataWidth -> "1",
      TagAttributes.DataHeight -> "1"
    ))

    embedTagValidator.validate("content", tag).size should be (0)
  }



}
