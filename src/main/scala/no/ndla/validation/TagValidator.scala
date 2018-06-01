/*
 * Part of NDLA validation.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.validation

import com.netaporter.uri.dsl._
import no.ndla.validation.EmbedTagRules.{ResourceHtmlEmbedTag}
import no.ndla.validation.TagRules.TagAttributeRules
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._

class TagValidator {
  def validate(fieldName: String, content: String): Seq[ValidationMessage] = {
    val document = HtmlTagRules.stringToJsoupDocument(content)
    document.select("*").asScala.flatMap((tag) => {
      if (tag.tagName == ResourceHtmlEmbedTag) validateEmbedTag(fieldName, tag) else validateHtmlTag(fieldName, tag)
    }).toList

  }

  def validateHtmlTag(fieldName: String, html: Element): Seq[ValidationMessage] = {
    val tagName = html.tagName
    if (!HtmlTagRules.isTagValid(tagName)) {
      return Seq.empty
    }

    val allAttributesOnTag = html.attributes().asScala.map(attr => attr.getKey -> attr.getValue).toMap
    val legalAttributesUsed = getLegalAttributesUsed(allAttributesOnTag, tagName)
    val validationErrors = attributesAreLegal(fieldName, allAttributesOnTag, tagName)

    val legalAttributesForTag = HtmlTagRules.tagAttributesForTagType(html.tagName).getOrElse(TagAttributeRules.empty)


    val missingAttributes =  getMissingAttributes(legalAttributesForTag.required, legalAttributesUsed.keys.toSet)
    val missingErrors = missingAttributes.map(missingAttributes => ValidationMessage(fieldName, s"$tagName must contain the following attributes: ${legalAttributesForTag.required.mkString(",")}. " +
      s"Optional attributes are: ${legalAttributesForTag.optional.mkString(",")}. " +
      s"Missing: ${missingAttributes.mkString(",")}")).toList
    val partialErrorMessage = s"A $tagName HTML tag"

    val optionalErrors =  verifyOptionals(fieldName, legalAttributesForTag, legalAttributesUsed.keys.toSet, partialErrorMessage)

    validationErrors ++ missingErrors ++ optionalErrors
  }

  private def validateEmbedTag(fieldName: String, embed: Element): Seq[ValidationMessage] = {
    if (embed.tagName != ResourceHtmlEmbedTag)
      return Seq()

    val allAttributesOnTag = embed.attributes().asScala.map(attr => attr.getKey -> attr.getValue).toMap
    val legalAttributes = getLegalAttributesUsed(allAttributesOnTag, ResourceHtmlEmbedTag)

    val validationErrors = attributesAreLegal(fieldName, allAttributesOnTag, ResourceHtmlEmbedTag) ++
      attributesContainsNoHtml(fieldName, legalAttributes) ++
      verifyAttributeResource(fieldName, legalAttributes)

    validationErrors.toList
  }


  private def attributesAreLegal(fieldName: String, attributes: Map[String, String], tagName: String): List[ValidationMessage] = {
    val legalAttributeKeys = HtmlTagRules.legalAttributesForTag(tagName)
    val illegalAttributesUsed: Set[String] = attributes.keySet diff legalAttributeKeys
    val legalAttributesUsed: Set[String] = attributes.keySet diff illegalAttributesUsed

    val illegalTagsError = if (illegalAttributesUsed.nonEmpty) {
      List(ValidationMessage(fieldName,
        s"An HTML tag '$tagName' contains an illegal attribute(s) '${illegalAttributesUsed.mkString(",")}'. Allowed attributes are ${legalAttributeKeys.mkString(",")}"))
    } else {
      List.empty
    }

    val mustContainAttributesError = if (HtmlTagRules.tagMustContainAtLeastOneAttribute(tagName) && legalAttributesUsed.isEmpty) {
      List(ValidationMessage(fieldName, s"An HTML tag '$tagName' must contain at least one attribute"))
    } else {
      List.empty
    }

    illegalTagsError ++ mustContainAttributesError
  }

  private def attributesContainsNoHtml(fieldName: String, attributes: Map[TagAttributes.Value, String]): Option[ValidationMessage] = {
    val attributesWithHtml = attributes.toList.filter{ case (_, value) =>
      new TextValidator(allowHtml=false).validate(fieldName, value).nonEmpty
    }.toMap.keySet

    if (attributesWithHtml.nonEmpty) {
      Some(ValidationMessage(fieldName, s"HTML tag '$ResourceHtmlEmbedTag' contains attributes with HTML: ${attributesWithHtml.mkString(",")}"))
    } else {
      None
    }
  }

  private def verifyAttributeResource(fieldName: String, attributes: Map[TagAttributes.Value, String]): Seq[ValidationMessage] = {
    val attributeKeys = attributes.keySet
    if (!attributeKeys.contains(TagAttributes.DataResource)) {
      return ValidationMessage(fieldName, s"$ResourceHtmlEmbedTag tags must contain a ${TagAttributes.DataResource} attribute") :: Nil
    }

    if (!ResourceType.all.contains(attributes(TagAttributes.DataResource))) {
      return Seq(ValidationMessage(fieldName, s"The ${TagAttributes.DataResource} attribute can only contain one of the following values: ${ResourceType.all.mkString(",")}"))
    }

    val resourceType = ResourceType.valueOf(attributes(TagAttributes.DataResource)).get

    attributes.get(TagAttributes.DataResource).map(dr => {
      ResourceType.valueOf(dr) match {
        case Some(dataResource) =>
          val attributeRulesForTag = EmbedTagRules.attributesForResourceType(resourceType)

          val partialErrorMessage = s"An $ResourceHtmlEmbedTag HTML tag with ${TagAttributes.DataResource}=$resourceType"

          verifyEmbedTagBasedOnResourceType(fieldName, attributeRulesForTag, attributeKeys, resourceType) ++
            verifyOptionals(fieldName, attributeRulesForTag, attributeKeys, partialErrorMessage) ++
            verifySourceUrl(fieldName, attributeRulesForTag, attributes, resourceType)
        case _ =>
          Seq(ValidationMessage(fieldName, "Something went wrong when determining resourceType of embed"))
      }
    })
    .getOrElse(Seq(
      ValidationMessage(fieldName,
                        s"Embed-tag did not contain any ${TagAttributes.DataResource.toString} attribute.")))

  }

  private def verifyEmbedTagBasedOnResourceType(fieldName: String, attrRules: TagAttributeRules, actualAttributes: Set[TagAttributes.Value], resourceType: ResourceType.Value): Seq[ValidationMessage] = {
    val missingAttributes = getMissingAttributes(attrRules.required, actualAttributes)
    val illegalAttributes = getMissingAttributes(actualAttributes, attrRules.all)

    val partialErrorMessage = s"An $ResourceHtmlEmbedTag HTML tag with ${TagAttributes.DataResource}=$resourceType"
    missingAttributes.map(missingAttributes => ValidationMessage(fieldName, s"$partialErrorMessage must contain the following attributes: ${attrRules.required.mkString(",")}. " +
      s"Optional attributes are: ${attrRules.optional.mkString(",")}. " +
      s"Missing: ${missingAttributes.mkString(",")}")).toList ++
      illegalAttributes.map(illegalAttributes => ValidationMessage(fieldName, s"$partialErrorMessage can not contain any of the following attributes: ${illegalAttributes.mkString(",")}"))
  }

  private def verifyOptionals(fieldName: String,
                              attrsRules: TagAttributeRules,
                              actualAttributes: Set[TagAttributes.Value],
                              partialErrorMessage: String): Seq[ValidationMessage] = {

    val usedOptionalAttr = actualAttributes.intersect(attrsRules.optional.flatten.toSet)
    val a = usedOptionalAttr.flatMap(attr => {
      val attrRuleGroup = attrsRules.optional.find(_.contains(attr))
      attrRuleGroup.map(attrRules => verifyUsedAttributesAgainstAttrRules(fieldName, attrRules, usedOptionalAttr, partialErrorMessage))
    }).toSeq
    a.flatten
  }

  private def verifyUsedAttributesAgainstAttrRules(fieldName: String,
                                                   attrRules: Set[TagAttributes.Value],
                                                   usedOptionalAttrs: Set[TagAttributes.Value],
                                                   partialErrorMessage: String): Seq[ValidationMessage] = {
    val usedOptionalAttrsInCurrentGroup = usedOptionalAttrs.intersect(attrRules)
    usedOptionalAttrsInCurrentGroup.isEmpty match {
      case false if usedOptionalAttrsInCurrentGroup != attrRules =>
        val missingAttrs = attrRules.diff(usedOptionalAttrs).mkString(",")
        Seq(ValidationMessage(fieldName,
          s"$partialErrorMessage must contain all or none of the optional attributes (${attrRules.mkString(",")}). Missing $missingAttrs"
        ))
      case _ => Seq.empty
    }
  }

  private def verifySourceUrl(fieldName: String, attrs: TagAttributeRules, usedAttributes: Map[TagAttributes.Value, String], resourceType: ResourceType.Value): Seq[ValidationMessage] = {
    usedAttributes.get(TagAttributes.DataUrl) match {
      case Some(url) if attrs.validSrcDomains.nonEmpty && !attrs.validSrcDomains.get.exists(url.host.getOrElse("").matches) =>
        Seq(ValidationMessage(fieldName,
          s"An $ResourceHtmlEmbedTag HTML tag with ${TagAttributes.DataResource}=$resourceType can only contain ${TagAttributes.DataUrl} urls from the following domains: ${attrs.validSrcDomains.mkString(",")}"))
      case _ => Seq.empty
    }
  }

  private def getMissingAttributes(requiredAttributes: Set[TagAttributes.Value], attributeKeys: Set[TagAttributes.Value]) = {
    val missing = requiredAttributes diff attributeKeys
    missing.headOption.map(_ => missing)
  }

  private def getLegalAttributesUsed(allAttributes: Map[String, String], tagName: String): Map[TagAttributes.Value, String] = {
    val legalAttributeKeys = HtmlTagRules.legalAttributesForTag(tagName)

    allAttributes.filter { case (key, _) => legalAttributeKeys.contains(key) }
      .map {
        case (key, value) =>
          TagAttributes.valueOf(key).getOrElse(TagAttributes.getOrCreate(key)) -> value
      }
  }

}
