module Marksman.Symbols

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Names
open Marksman.Cst
open Marksman.Doc
open Marksman.Index
open Marksman.Folder
open Marksman.Workspace

let headingToSymbolName (h: Node<Heading>) : string = $"H{h.data.level}: {Heading.name h.data}"

let headingToSymbolInfo (docUri: DocId) (h: Node<Heading>) : SymbolInformation =
    let name = headingToSymbolName h
    let kind = SymbolKind.String
    let location = { Uri = docUri.Uri; Range = h.range }

    let sym = {
        Name = name
        Kind = kind
        Location = location
        ContainerName = None
        Tags = None
        Deprecated = None
    }

    sym

let rec headingToDocumentSymbol (isEmacs: bool) (cst: Cst) (h: Node<Heading>) : DocumentSymbol =
    let name = Heading.name h.data
    let kind = SymbolKind.String
    let range = h.data.scope
    let selectionRange = h.range

    let toDocumentSymbol (e: Element) : option<DocumentSymbol> =
        match e with
        | H h -> Some(headingToDocumentSymbol isEmacs cst h)
        | _ -> None

    let children =
        Cst.children cst (H h)
        |> Array.map toDocumentSymbol
        |> Array.collect Option.toArray

    let children =
        if Array.isEmpty children then
            None
        // Emacs' imenu with consult/counsel/etc. doesn't allow selecting intermediate
        // nodes that have children. As a workaround we add a '.' this node.
        else if isEmacs then
            let thisHeading = {
                Name = "."
                Detail = None
                Kind = kind
                Range = selectionRange
                SelectionRange = selectionRange
                Children = None
                Tags = None
                Deprecated = None
            }

            Some(Array.append [| thisHeading |] children)
        else
            Some children

    {
        Name = name
        Detail = None
        Kind = kind
        Range = range
        SelectionRange = selectionRange
        Children = children
        Tags = None
        Deprecated = None
    }

let docSymbols
    (hierarchy: bool)
    (isEmacs: bool)
    (doc: Doc)
    : U2<array<SymbolInformation>, array<DocumentSymbol>> =
    if hierarchy then
        let cst = Doc.cst doc
        let topLevelHeadings = Cst.topLevelHeadings cst

        topLevelHeadings
        |> Seq.map (headingToDocumentSymbol isEmacs cst)
        |> Array.ofSeq
        |> Second
    else
        Doc.index >> Index.headings <| doc
        |> Seq.map (headingToSymbolInfo (Doc.id doc))
        |> Array.ofSeq
        |> First

let workspaceSymbols (query: string) (ws: Workspace) : array<SymbolInformation> =
    seq {
        for folder in Workspace.folders ws do
            for doc in Folder.docs folder do
                let matchingHeadingSymbols =
                    Doc.index doc
                    |> Index.headings
                    |> Seq.filter (headingToSymbolName >> query.IsSubSequenceOf)
                    |> Seq.map (headingToSymbolInfo (Doc.id doc))

                yield! matchingHeadingSymbols
    }
    |> Array.ofSeq
