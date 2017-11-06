/*
 * Part of NDLA validation.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.validation

import no.ndla.mapping.UnitSuite

class EmbedTagRulesTest extends UnitSuite {

  test("Rules for all resource types should be defined") {
    val resourceTypesFromConfigFile = EmbedTagRules.attributeRules.keys
    val resourceTypesFromEnumDeclaration = ResourceType.values

    resourceTypesFromEnumDeclaration should equal (resourceTypesFromConfigFile)
  }

  test("data-resource should be required for all resource types") {
    val resourceTypesFromConfigFile = EmbedTagRules.attributeRules.keys

    resourceTypesFromConfigFile.foreach(resType =>
      EmbedTagRules.attributesForResourceType(resType).required should contain(Attributes.DataResource)
    )
  }

}
