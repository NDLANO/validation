/*
 * Part of NDLA validation.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.validation

import scala.language.postfixOps
import scala.io.Source

object HtmlTagRules {

  case class HtmlThings(attrsForResource: Map[String, TagRules.TagAttributeRules])

  private[validation] lazy val attributeRules: Map[String, TagRules.TagAttributeRules] = tagRulesToJson
  lazy val allHtmlTagAttributes: Set[TagAttributes.Value] = attributeRules.flatMap { case (_ , attrRules)  => attrRules.all } toSet

  private def tagRulesToJson = {
    val attrs = TagRules.convertJsonStr(Source.fromResource("html-rules.json").mkString)
      .get("attributes").map(_.asInstanceOf[Map[String, Map[String, Any]]])
    attrs.get.map {
      case (tagType, attrRules) => {
        tagType -> TagRules.toTagAttributeRules(attrRules)
      }
    }
  }
}
