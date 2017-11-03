/*
 * Part of NDLA validation.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.validation

import scala.io.Source
import EmbedTagRules.ResourceHtmlEmbedTag
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.nodes.Entities.EscapeMode
import scala.collection.JavaConverters._

object HtmlRules {
  def stringToJsoupDocument(htmlString: String): Element = {
    val document = Jsoup.parseBodyFragment(htmlString)
    document.outputSettings().escapeMode(EscapeMode.xhtml).prettyPrint(false)
    document.select("body").first()
  }

  def jsoupDocumentToString(element: Element): String = {
    element.select("body").html()
  }

  object PermittedHTML {
    val attributes: Map[String, Seq[String]] = readAttributes
    val tags: Set[String] = readTags

    private def convertJsonStr(jsonStr: String): Map[String, Any] = {
      implicit val formats = org.json4s.DefaultFormats
      parse(jsonStr).extract[Map[String, Any]]
    }

    private def htmlRulesJson: Map[String, Any] = convertJsonStr(Source.fromResource("html-rules.json").mkString)

    private def mathMLRulesJson: Map[String, Any] = convertJsonStr(Source.fromResource("mathml-rules.json").mkString)

    private def readAttributes: Map[String, Seq[String]] = {
      val htmlJson: Map[String, Any] = htmlRulesJson
      val mathMlJson: Map[String, Any] = mathMLRulesJson

      val htmlAttr = htmlJson.get("attributes").map(_.asInstanceOf[Map[String, Seq[String]]])
      val mathMlAttrs = mathMlJson.get("attributes").map(_.asInstanceOf[Map[String, Seq[String]]])
      val embedAttrs = EmbedTagRules.allEmbedTagAttributes.map(_.toString).toSeq
      htmlAttr.getOrElse(Map.empty) ++ mathMlAttrs.getOrElse(Map.empty) ++ Map(ResourceHtmlEmbedTag -> embedAttrs)
    }

    private def readTags: Set[String] = {
      val htmlJson: Map[String, Any] = htmlRulesJson
      val mathMlJson: Map[String, Any] = mathMLRulesJson

      val htmlTags = htmlJson.get("tags").map(_.asInstanceOf[Seq[String]].toSet)
      val mathMlTags = mathMlJson.get("tags").map(_.asInstanceOf[Seq[String]].toSet)

      htmlTags.getOrElse(Set.empty) ++ mathMlTags.getOrElse(Set.empty) ++ attributes.keys
    }
  }

  def isAttributeKeyValid(attributeKey: String, tagName: String): Boolean = {
    val legalAttrs = legalAttributesForTag(tagName)
    legalAttrs.contains(attributeKey)
  }

  def isTagValid(tagName: String): Boolean = PermittedHTML.tags.contains(tagName)

  def allLegalTags: Set[String] = PermittedHTML.tags

  def legalAttributesForTag(tagName: String): Set[String] = {
    PermittedHTML.attributes.getOrElse(tagName, Seq.empty).toSet
  }

  def removeIllegalAttributes(el: Element, legalAttributes: Set[String]): Seq[String] = {
    el.attributes().asScala.toList.
      filter(attr => !legalAttributes.contains(attr.getKey))
      .map(illegalAttribute => {
        val keyName = illegalAttribute.getKey
        el.removeAttr(keyName)
        keyName
      })
  }
}
