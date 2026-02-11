module Marksman.MditaMap

open Markdig
open Markdig.Syntax
open Markdig.Syntax.Inlines

open Marksman.Text

/// A reference to a topic in an MDITA map.
type TopicRef = {
    href: string
    title: string
    children: list<TopicRef>
}

/// The parsed structure of an MDITA map file.
type MditaMapStructure = {
    title: option<string>
    topicRefs: list<TopicRef>
}

module MditaMapStructure =
    let empty = { title = None; topicRefs = [] }

/// Parse a .mditamap file content into an MditaMapStructure.
/// Map files use nested lists of links to define topic hierarchies:
/// ```
/// # Map Title
/// - [Topic 1](topic1.md)
///   - [Subtopic](sub.md)
/// - [Topic 2](topic2.md)
/// ```
let parseMap (text: Text) : MditaMapStructure =
    let pipeline =
        MarkdownPipelineBuilder()
            .UsePreciseSourceLocation()
            .Build()

    let parsed = Markdown.Parse(text.content, pipeline)

    let mutable mapTitle: option<string> = None

    // Extract the title from the first H1 heading
    for block in parsed do
        match block with
        | :? HeadingBlock as h when h.Level = 1 && mapTitle.IsNone ->
            let titleText = text.content.Substring(h.Span.Start, h.Span.Length)
            mapTitle <- Some(titleText.TrimStart(' ', '#').TrimEnd(' '))
        | _ -> ()

    let rec extractTopicRefs (block: Block) : list<TopicRef> =
        match block with
        | :? ListBlock as listBlock ->
            [ for item in listBlock do
                match item with
                | :? ListItemBlock as listItem ->
                    yield! extractFromListItem listItem
                | _ -> () ]
        | _ -> []

    and extractFromListItem (listItem: ListItemBlock) : list<TopicRef> =
        let mutable link: option<string * string> = None
        let mutable children: list<TopicRef> = []

        for subBlock in listItem do
            match subBlock with
            | :? ParagraphBlock as para ->
                // Look for a link inline
                if para.Inline <> null then
                    for inline_ in para.Inline do
                        match inline_ with
                        | :? LinkInline as linkInline when link.IsNone ->
                            let url = if linkInline.Url <> null then linkInline.Url else ""
                            let linkText =
                                if linkInline.LabelSpan.IsEmpty then
                                    url
                                else
                                    text.content.Substring(linkInline.LabelSpan.Start, linkInline.LabelSpan.Length)
                            link <- Some(url, linkText)
                        | _ -> ()
            | :? ListBlock as nestedList ->
                children <- extractTopicRefs nestedList
            | _ -> ()

        match link with
        | Some(href, title) ->
            [ { href = href; title = title; children = children } ]
        | None -> []

    let topicRefs =
        [ for block in parsed do
            match block with
            | :? ListBlock -> yield! extractTopicRefs block
            | _ -> () ]

    { title = mapTitle; topicRefs = topicRefs }
