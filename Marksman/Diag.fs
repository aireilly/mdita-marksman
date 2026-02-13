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
    | UnrecognizedSchema of schemaValue: string
    | TaskMissingProcedure
    | ConceptHasProcedure of Lsp.Range
    | ReferenceMissingTable
    | MapHasBodyContent
    | ExtendedFeatureInCoreProfile of featureName: string * Lsp.Range
    | FootnoteRefWithoutDef of label: string * Lsp.Range
    | FootnoteDefWithoutRef of label: string * Lsp.Range
    | UnknownAdmonitionType of admonitionType: string * Lsp.Range

let code: Entry -> string =
    function
    | AmbiguousLink _ -> "1"
    | BrokenLink _ -> "2"
    | NonBreakableWhitespace _ -> "3"
    | MissingYamlFrontMatter -> "4"
    | MissingShortDescription -> "5"
    | InvalidHeadingHierarchy _ -> "6"
    | UnrecognizedSchema _ -> "7"
    | TaskMissingProcedure -> "8"
    | ConceptHasProcedure _ -> "9"
    | ReferenceMissingTable -> "10"
    | MapHasBodyContent -> "11"
    | ExtendedFeatureInCoreProfile _ -> "12"
    | FootnoteRefWithoutDef _ -> "13"
    | FootnoteDefWithoutRef _ -> "14"
    | UnknownAdmonitionType _ -> "15"

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

let checkSchemaCompliance (doc: Doc) : list<Entry> =
    let index = Doc.index doc
    let bf = index.blockFeatures

    match index.yamlMetadata |> Option.bind (fun m -> m.schema) with
    | None -> []
    | Some schema ->
        let mutable entries = []

        match schema with
        | Cst.SchemaUnknown value ->
            entries <- UnrecognizedSchema value :: entries
        | Cst.SchemaTask ->
            if not bf.hasOrderedList then
                entries <- TaskMissingProcedure :: entries
        | Cst.SchemaConcept ->
            match bf.orderedListRanges with
            | range :: _ ->
                entries <- ConceptHasProcedure range :: entries
            | [] -> ()
        | Cst.SchemaReference ->
            if not bf.hasTable && not bf.hasDefinitionList then
                entries <- ReferenceMissingTable :: entries
        | Cst.SchemaMap ->
            // Map topics should primarily contain navigation links in lists.
            // If there are headings beyond the title, flag body content.
            let headings = Index.headings index
            let nonTitleHeadings = headings |> Array.filter (fun h -> not h.data.isTitle)

            if nonTitleHeadings.Length > 0 then
                entries <- MapHasBodyContent :: entries
        | Cst.SchemaTopic
        | Cst.SchemaMditaTopic
        | Cst.SchemaMditaCoreTopic
        | Cst.SchemaMditaExtendedTopic -> ()

        List.rev entries

let validDitaNoteTypes =
    set [ "note"; "tip"; "warning"; "caution"; "danger"; "attention"; "important"; "notice"; "fastpath"; "remember"; "restriction"; "trouble" ]

let checkExtendedProfileCompliance (doc: Doc) : list<Entry> =
    let index = Doc.index doc
    let bf = index.blockFeatures
    let mutable entries = []

    let schema = index.yamlMetadata |> Option.bind (fun m -> m.schema)

    // Profile gating: warn when Extended-only features appear in Core-profile docs
    match schema with
    | Some s when DitaSchema.isCore s ->
        if bf.hasDefinitionList then
            entries <- ExtendedFeatureInCoreProfile("Definition lists", Range.Mk(0, 0, 0, 0)) :: entries

        if bf.hasFootnoteRefs || bf.hasFootnoteDefs then
            entries <- ExtendedFeatureInCoreProfile("Footnotes", Range.Mk(0, 0, 0, 0)) :: entries

        if bf.hasStrikethrough then
            entries <- ExtendedFeatureInCoreProfile("Strikethrough", Range.Mk(0, 0, 0, 0)) :: entries

        if bf.hasGenericAttributes then
            entries <- ExtendedFeatureInCoreProfile("Generic attributes", Range.Mk(0, 0, 0, 0)) :: entries

        if not (List.isEmpty bf.admonitions) then
            entries <- ExtendedFeatureInCoreProfile("Admonitions", Range.Mk(0, 0, 0, 0)) :: entries
    | _ -> ()

    // Footnote validation: detect unmatched refs/defs (any profile)
    let defLabels = bf.footnoteDefLabels |> List.map fst |> set
    let refLabels = bf.footnoteRefLabels |> List.map fst |> set

    for (label, range) in bf.footnoteRefLabels do
        if not (Set.contains label defLabels) then
            entries <- FootnoteRefWithoutDef(label, range) :: entries

    for (label, range) in bf.footnoteDefLabels do
        if not (Set.contains label refLabels) then
            entries <- FootnoteDefWithoutRef(label, range) :: entries

    // Admonition type validation (any profile)
    for (admonType, range) in bf.admonitions do
        if not (Set.contains (admonType.ToLowerInvariant()) validDitaNoteTypes) then
            entries <- UnknownAdmonitionType(admonType, range) :: entries

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
                        yield! checkSchemaCompliance doc
                        yield! checkExtendedProfileCompliance doc
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
            Source = Some "MDITA Marksman"
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
            | YML _ -> Lsp.DiagnosticSeverity.Information

        let msg = $"Link to non-existent {refToHuman ref}"

        {
            Range = Element.range el
            Severity = Some severity
            Code = Some(code diag)
            CodeDescription = None
            Source = Some "MDITA Marksman"
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
        Source = Some "MDITA Marksman"
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
        Source = Some "MDITA Marksman"
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
        Source = Some "MDITA Marksman"
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
        Source = Some "MDITA Marksman"
        Message =
            $"MDITA: Heading level {currentLevel} skips levels. Expected at most level {expectedMaxLevel}."
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | UnrecognizedSchema schemaValue -> {
        Range = Range.Mk(0, 0, 0, 0)
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = $"Unrecognized $schema: '{schemaValue}'"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | TaskMissingProcedure -> {
        Range = Range.Mk(0, 0, 0, 0)
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = "Task topic should contain procedure steps (ordered list)"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | ConceptHasProcedure procRange -> {
        Range = procRange
        Severity = Some Lsp.DiagnosticSeverity.Information
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message =
            "Concept topic contains ordered list; consider using task schema if these are procedure steps"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | ReferenceMissingTable -> {
        Range = Range.Mk(0, 0, 0, 0)
        Severity = Some Lsp.DiagnosticSeverity.Information
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = "Reference topic typically contains tables or definition lists"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | MapHasBodyContent -> {
        Range = Range.Mk(0, 0, 0, 0)
        Severity = Some Lsp.DiagnosticSeverity.Information
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = "Map topic should primarily contain navigation links in lists"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | ExtendedFeatureInCoreProfile(featureName, range) -> {
        Range = range
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = $"'{featureName}' requires MDITA Extended Profile; document uses Core Profile"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | FootnoteRefWithoutDef(label, range) -> {
        Range = range
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = $"Footnote reference '[^{label}]' has no matching definition"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | FootnoteDefWithoutRef(label, range) -> {
        Range = range
        Severity = Some Lsp.DiagnosticSeverity.Information
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = $"Footnote definition '[^{label}]' is not referenced"
        RelatedInformation = None
        Tags = None
        Data = None
      }
    | UnknownAdmonitionType(admonType, range) -> {
        Range = range
        Severity = Some Lsp.DiagnosticSeverity.Warning
        Code = Some(code diag)
        CodeDescription = None
        Source = Some "MDITA Marksman"
        Message = $"Unknown admonition type '{admonType}'; DITA supports: note, tip, warning, caution, danger, attention, important, notice, remember, restriction, trouble"
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
