/*
 * Part of NDLA validation.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.validation

import no.ndla.mapping.UnitSuite

class HtmlRulesTest extends UnitSuite {
  test("embed tag should be an allowed tag and contain data attributes") {
    HtmlRules.isTagValid("embed")

    val dataAttrs = Attributes.values.map(_.toString).filter(x => x.startsWith("data-") && x != Attributes.DataType.toString)
    val legalEmbedAttrs = HtmlRules.legalAttributesForTag("embed")

    dataAttrs.foreach(x => legalEmbedAttrs should contain(x))
  }

  test("That isAttributeKeyValid returns false for illegal attributes") {
    HtmlRules.isAttributeKeyValid("data-random-junk", "td") should equal(false)
  }

  test("That isAttributeKeyValid returns true for legal attributes") {
    HtmlRules.isAttributeKeyValid("align", "td") should equal(true)
  }

  test("That isTagValid returns false for illegal tags") {
    HtmlRules.isTagValid("yodawg") should equal(false)
  }

  test("That isTagValid returns true for legal attributes") {
    HtmlRules.isTagValid("section") should equal(true)
  }

}
