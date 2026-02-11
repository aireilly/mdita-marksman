module Marksman.MditaMapTests

open Xunit
open Marksman.MditaMap

[<Fact>]
let testParseSimpleMap () =
    let content =
        """# My Map

- [Topic 1](topic1.md)
- [Topic 2](topic2.md)
"""

    let text = Text.mkText content
    let map = parseMap text

    Assert.Equal(Some "My Map", map.title)
    Assert.Equal(2, List.length map.topicRefs)
    Assert.Equal("topic1.md", map.topicRefs.[0].href)
    Assert.Equal("Topic 1", map.topicRefs.[0].title)
    Assert.Equal("topic2.md", map.topicRefs.[1].href)
    Assert.Equal("Topic 2", map.topicRefs.[1].title)

[<Fact>]
let testParseNestedMap () =
    let content =
        """# Nested Map

- [Parent](parent.md)
  - [Child 1](child1.md)
  - [Child 2](child2.md)
- [Other](other.md)
"""

    let text = Text.mkText content
    let map = parseMap text

    Assert.Equal(Some "Nested Map", map.title)
    Assert.Equal(2, List.length map.topicRefs)

    let parent = map.topicRefs.[0]
    Assert.Equal("parent.md", parent.href)
    Assert.Equal(2, List.length parent.children)
    Assert.Equal("child1.md", parent.children.[0].href)
    Assert.Equal("child2.md", parent.children.[1].href)

    Assert.Equal(0, List.length map.topicRefs.[1].children)

[<Fact>]
let testParseMapNoTitle () =
    let content =
        """- [Topic](topic.md)
"""

    let text = Text.mkText content
    let map = parseMap text

    Assert.Equal(None, map.title)
    Assert.Equal(1, List.length map.topicRefs)

[<Fact>]
let testParseEmptyMap () =
    let content = ""
    let text = Text.mkText content
    let map = parseMap text

    Assert.Equal(None, map.title)
    Assert.Equal(0, List.length map.topicRefs)

[<Fact>]
let testParseDeeplyNested () =
    let content =
        """# Deep Map

- [Level 1](l1.md)
  - [Level 2](l2.md)
    - [Level 3](l3.md)
"""

    let text = Text.mkText content
    let map = parseMap text

    Assert.Equal(1, List.length map.topicRefs)
    let l1 = map.topicRefs.[0]
    Assert.Equal("l1.md", l1.href)
    Assert.Equal(1, List.length l1.children)
    let l2 = l1.children.[0]
    Assert.Equal("l2.md", l2.href)
    Assert.Equal(1, List.length l2.children)
    let l3 = l2.children.[0]
    Assert.Equal("l3.md", l3.href)
    Assert.Equal(0, List.length l3.children)
