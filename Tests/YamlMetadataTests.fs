module Marksman.YamlMetadataTests

open Xunit
open Marksman.Cst

[<Fact>]
let testParseBasicMetadata () =
    let yaml =
        """---
author: John Doe
source: https://example.com
publisher: ACME Corp
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some "John Doe", meta.author)
    Assert.Equal(Some "https://example.com", meta.source)
    Assert.Equal(Some "ACME Corp", meta.publisher)

[<Fact>]
let testParseAllKnownKeys () =
    let yaml =
        """---
author: Jane
source: src
publisher: pub
permissions: all
audience: developers
category: tools
keyword: markdown
resourceid: res-001
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some "Jane", meta.author)
    Assert.Equal(Some "src", meta.source)
    Assert.Equal(Some "pub", meta.publisher)
    Assert.Equal(Some "all", meta.permissions)
    Assert.Equal(Some "developers", meta.audience)
    Assert.Equal(Some "tools", meta.category)
    Assert.Equal(Some [ "markdown" ], meta.keyword)
    Assert.Equal(Some "res-001", meta.resourceid)

[<Fact>]
let testParseInlineArray () =
    let yaml =
        """---
keyword: [markdown, dita, mdita]
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some [ "markdown"; "dita"; "mdita" ], meta.keyword)

[<Fact>]
let testParseMultiLineArray () =
    let yaml =
        """---
keyword:
- markdown
- dita
- mdita
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some [ "markdown"; "dita"; "mdita" ], meta.keyword)

[<Fact>]
let testParseQuotedValues () =
    let yaml =
        """---
author: "John Doe"
keyword: ['foo', "bar"]
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some "John Doe", meta.author)
    Assert.Equal(Some [ "foo"; "bar" ], meta.keyword)

[<Fact>]
let testParseEmptyYaml () =
    let yaml = "---\n---"
    let meta = YamlMetadata.parse yaml
    Assert.Equal(None, meta.author)
    Assert.Equal(None, meta.keyword)
    Assert.True(Map.isEmpty meta.otherMeta)

[<Fact>]
let testParseMissingFields () =
    let yaml =
        """---
author: Someone
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some "Someone", meta.author)
    Assert.Equal(None, meta.source)
    Assert.Equal(None, meta.publisher)
    Assert.Equal(None, meta.permissions)
    Assert.Equal(None, meta.audience)
    Assert.Equal(None, meta.category)
    Assert.Equal(None, meta.keyword)
    Assert.Equal(None, meta.resourceid)

[<Fact>]
let testParseUnknownKeys () =
    let yaml =
        """---
author: Someone
custom_key: custom_value
another: thing
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some "Someone", meta.author)
    Assert.Equal(Some "custom_value", Map.tryFind "custom_key" meta.otherMeta)
    Assert.Equal(Some "thing", Map.tryFind "another" meta.otherMeta)

[<Fact>]
let testParseNoDelimiters () =
    let yaml = "author: Someone\nkeyword: test"
    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some "Someone", meta.author)
    Assert.Equal(Some [ "test" ], meta.keyword)

[<Fact>]
let testParseSchemaConceptXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:xsd:concept.xsd
author: test
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaConcept, meta.schema)
    Assert.Equal(Some "test", meta.author)

[<Fact>]
let testParseSchemaTaskXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:xsd:task.xsd
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaTask, meta.schema)

[<Fact>]
let testParseSchemaReferenceXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:xsd:reference.xsd
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaReference, meta.schema)

[<Fact>]
let testParseSchemaMapXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:xsd:map.xsd
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaMap, meta.schema)

[<Fact>]
let testParseSchemaTopicXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:xsd:topic.xsd
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaTopic, meta.schema)

[<Fact>]
let testParseSchemaMditaTopicXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:mdita:xsd:topic.xsd
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaMditaTopic, meta.schema)

[<Fact>]
let testParseSchemaMditaCoreTopicXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:mdita:core:xsd:topic.xsd
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaMditaCoreTopic, meta.schema)

[<Fact>]
let testParseSchemaMditaExtendedTopicXsd () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaMditaExtendedTopic, meta.schema)

[<Fact>]
let testParseSchemaConceptRng () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:rng:concept.rng
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaConcept, meta.schema)

[<Fact>]
let testParseSchemaTaskRng () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:rng:task.rng
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaTask, meta.schema)

[<Fact>]
let testParseSchemaUnknownValue () =
    let yaml =
        """---
$schema: urn:example:custom:schema
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some(DitaSchema.SchemaUnknown "urn:example:custom:schema"), meta.schema)

[<Fact>]
let testParseSchemaNotInOtherMeta () =
    let yaml =
        """---
$schema: urn:oasis:names:tc:dita:xsd:task.xsd
custom: value
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(Some DitaSchema.SchemaTask, meta.schema)
    // $schema should not appear in otherMeta
    Assert.Equal(None, Map.tryFind "$schema" meta.otherMeta)
    Assert.Equal(Some "value", Map.tryFind "custom" meta.otherMeta)

[<Fact>]
let testParseNoSchema () =
    let yaml =
        """---
author: test
---"""

    let meta = YamlMetadata.parse yaml
    Assert.Equal(None, meta.schema)
