module Marksman.Diag

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names
open Marksman.Doc
open Marksman.Folder
open Marksman.Workspace

module Lsp = Ionide.LanguageServerProtocol.Types

open Marksman.Cst
open Marksman.Index
open Marksman.Refs

type Entry =
    | AmbiguousLink of Element * Syms.Ref * array<Dest>
    | BrokenLink of Element * Syms.Ref
    | NonBreakableWhitespace of Lsp.Range
    | MissingYamlFrontMatter
    | MissingShortDescription
    | InvalidHeadingHierarchy of Lsp.Range * currentLevel: int * expectedMaxLevel: int

let code: Entry -> string =
    function
    | AmbiguousLink _ -> "1"
    | BrokenLink _ -> "2"
    | NonBreakableWhitespace _ -> "3"
    | MissingYamlFrontMatter -> "4"
    | MissingShortDescription -> "5"
    | InvalidHeadingHierarchy _ -> "6"

let checkNonBreakingWhitespace (doc: Doc) =
    let nonBreakingWhitespace = "\u00a0"

    let headings = [ 1..7 ] |> List.map (fun n -> (String.replicate n "#"))

    [ 0 .. (Doc.text doc).lineMap.NumLines ]
    |> List.collect (fun x ->
        let line = (Doc.text doc).LineContent x

        let headingLike =
            List.tryFind (fun (h: string) -> line.StartsWith(h + nonBreakingWhitespace)) headings

        match headingLike with
        | None -> []
        | Some heading ->
            let whitespaceRange: Lsp.Range = {
                Start = { Line = x; Character = heading.Length }
                End = { Line = x; Character = heading.Length + 1 }
            }

            [ NonBreakableWhitespace(whitespaceRange) ])

let checkLink (folder: Folder) (doc: Doc) (linkEl: Element) : seq<Entry> =
    let exts = Folder.configuredMarkdownExts folder
    let config = Folder.configOrDefault folder
    let mditaEnabled = config.CoreMditaEnable()

    let ref =
        doc.Structure
        |> Structure.Structure.tryFindSymbolForConcrete linkEl
        |> Option.bind Syms.Sym.asRef

    match ref with
    | None -> []
    | Some ref ->
        let refs = Dest.tryResolveElement folder doc linkEl |> Array.ofSeq

        if Folder.isSingleFile folder && Syms.Ref.isCross ref then
            []
        else if refs.Length = 1 then
            []
        else if refs.Length = 0 then
            match linkEl with
            // In MDITA mode, shortcut reference links [key] become keyrefs
            // and should produce warnings for unresolved references.
            // In standard mode, suppress diagnostics as they are often regular text.
            | ML { data = MdLink.RS _ } ->
                if mditaEnabled then
                    [ BrokenLink(linkEl, ref) ]
                else
                    []
            | ML { data = MdLink.IL(_, url, _) } ->
                match url with
                | Some { data = url } ->
                    // Inline links to docs that don't look like a markdown file should not
                    // produce diagnostics
                    if Misc.isMarkdownFile exts (UrlEncoded.decode url) then
                        [ BrokenLink(linkEl, ref) ]
                    else
                        []
                | _ -> [ BrokenLink(linkEl, ref) ]
            | _ -> [ BrokenLink(linkEl, ref) ]
        else
            [ AmbiguousLink(linkEl, ref, refs) ]

let checkMditaCompliance (doc: Doc) : list<Entry> =
    let index = Doc.index doc
    let mutable entries = []

    // Check for YAML front matter
    if index.yamlFrontMatter.IsNone then
        entries <- MissingYamlFrontMatter :: entries

    // Check for short description (first paragraph after H1)
    if index.shortDescription.IsNone && (Index.titles index |> Array.isEmpty |> not) then
        entries <- MissingShortDescription :: entries

    // Check heading hierarchy - levels should not skip (e.g. H1 -> H4 without H2/H3)
    let headings = Index.headings index
    let mutable prevLevel = 0

    for h in headings do
        let level = h.data.level

        if level > prevLevel + 1 && prevLevel > 0 then
            entries <-
                InvalidHeadingHierarchy(h.range, level, prevLevel + 1)
                :: entries

        prevLevel <- level

    List.rev entries

let checkLinks (folder: Folder) (doc: Doc) : seq<Entry> =
    let links = Doc.index >> Index.links <| doc
    links |> Seq.collect (checkLink folder doc)

let checkFolder (folder: Folder) : seq<DocId * list<Entry>> =
    let config = Folder.configOrDefault folder
    let mditaEnabled = config.CoreMditaEnable()

    seq {
        for doc in Folder.docs folder do
            let docDiag =
                seq {
                    yield! checkLinks folder doc
                    yield! checkNonBreakingWhitespace doc

                    if mditaEnabled then
                        yield! checkMditaCompliance doc
                }
                |> List.ofSeq

            Doc.id doc, docDiag
    }

let destToHuman (ref: Dest) : string =
    match ref with
    | Dest.Doc { doc = doc } -> $"document {Doc.name doc}"
    | Dest.Heading(docLink, { data = heading }) ->
        $"heading {Heading.name heading} in the document {Doc.name (DocLink.doc docLink)}"
    | Dest.LinkDef(_, { data = ld }) -> $"link definition {MdLinkDef.name ld}"
    | Dest.Tag(doc, { data = tag }) -> $"tag {tag.name} in the document {Doc.name doc}"

let docToHuman (name: string) : string = $"document '{name}'"

let refToHuman (ref: Syms.Ref) : string =
    match ref with
    | Syms.CrossRef(Syms.CrossDoc docName) -> docToHuman docName
    | Syms.CrossRef(Syms.CrossSection(docName, sectionName)) ->
        $"heading '{Slug.toString sectionName}' in {docToHuman docName}"
    | Syms.IntraRef(Syms.IntraSection heading) -> $"heading '{Slug.toString heading}'"
    | Syms.IntraRef(Syms.IntraLinkDef ld) -> $"link definition with the label '{ld}'"

let diagToLsp (diag: Entry) : Lsp.Diagnostic =
    match diag with
    | AmbiguousLink(el, ref, dests) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | T _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let mkRelated dest : DiagnosticRelatedInformation =
            let loc = Dest.location dest
            let msg = $"Duplicate definition of {refToHuman ref}"
            { Location = loc; Message = msg }

        let related = dests |> Array.map mkRelated

        {
            Range = Element.range el
            Severity = Some severity
            Code = Some(code diag)
            CodeDescription = None
            Source = Some "Marksman"
            Message = $"Ambiguous link to {refToHuman ref}"
            RelatedInformation = Some related
            Tags = None
            Data = None
        }
    | BrokenLink(el, ref) ->
        let severity =
            match el with
            | WL _ -> Lsp.DiagnosticSeverity.Error
            | ML _ -> Lsp.DiagnosticSeverity.Warning
            | H _
            | MLD _
            | T _
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let msg = $"Link to non-existent {refToHuman ref}"

        {
            Range = Element.range el
            Severity = Some severity
            Code = Some(code diag)
            CodeDescription = None
            Source = Some "Marksman"
            Message = msg
            RelatedInformation = None
            Tags = None
            Data = None
        }

    | NonBreakableWhitespace dup -> {
        Range = dup
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "Marksman"
        Message =
            "Non-breaking whitespace used instead of regular whitespace. This line won't be interpreted as a heading"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | MissingYamlFrontMatter -> {
        Range = Range.Mk(0, 0, 0, 0)
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "Marksman"
        Message = "MDITA: Missing YAML front matter. Add a front matter block with '---' delimiters."
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | MissingShortDescription -> {
        Range = Range.Mk(0, 0, 0, 0)
        Severity = Some Lsp.DiagnosticSeverity.Information
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "Marksman"
        Message = "MDITA: Missing short description. Add a paragraph immediately after the title heading."
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | InvalidHeadingHierarchy(headingRange, currentLevel, expectedMaxLevel) -> {
        Range = headingRange
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "Marksman"
        Message =
            $"MDITA: Heading level {currentLevel} skips levels. Expected at most level {expectedMaxLevel}."
        RelatedInformation = None
        Tags = None
        Data = None
      }

type FolderDiag = array<DocId * array<Lsp.Diagnostic>>

module FolderDiag =
    let mk (folder: Folder) : FolderDiag =
        checkFolder folder
        |> Seq.map (fun (uri, diags) ->
            let lspDiags = List.map diagToLsp diags |> Array.ofList

            uri, lspDiags)
        |> Array.ofSeq

type WorkspaceDiag = Map<FolderId, FolderDiag>

module WorkspaceDiag =
    let mk (ws: Workspace) : WorkspaceDiag =
        Workspace.folders ws
        |> Seq.map (fun folder -> (Folder.id folder), FolderDiag.mk folder)
        |> Map.ofSeq

    let empty = Map.empty
