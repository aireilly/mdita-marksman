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
