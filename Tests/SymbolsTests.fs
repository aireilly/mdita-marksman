module Marksman.SymbolsTests

open Xunit

open Ionide.LanguageServerProtocol.Types

open Marksman.Misc
open Marksman.Helpers
open Marksman.Workspace

module WorkspaceSymbol =
    let mkSymbolInfo name kind loc = {
        Name = name
        Kind = kind
        Location = loc
        Tags = None
        Deprecated = None
        ContainerName = None
    }

    let doc1 =
        FakeDoc.Mk(path = "doc1.md", contentLines = [| "# A" |])

    let doc2 =
        FakeDoc.Mk(path = "doc2.md", contentLines = [| "# B" |])

    let folder = FakeFolder.Mk [ doc1; doc2 ]
    let workspace = Workspace.ofFolders None [ folder ]

    [<Fact>]
    let symbols_noQuery () =
        let symbols = Symbols.workspaceSymbols "" workspace

        Assert.Equal(
            [
                mkSymbolInfo "H1: A" SymbolKind.String {
                    Uri = doc1.Id.Uri
                    Range = Range.Mk(0, 0, 0, 3)
                }
                mkSymbolInfo "H1: B" SymbolKind.String {
                    Uri = doc2.Id.Uri
                    Range = Range.Mk(0, 0, 0, 3)
                }
            ],
            symbols
        )

    [<Fact>]
    let symbols_withQuery () =
        let symbols = Symbols.workspaceSymbols "H1:" workspace

        Assert.Equal(
            [
                mkSymbolInfo "H1: A" SymbolKind.String {
                    Uri = doc1.Id.Uri
                    Range = Range.Mk(0, 0, 0, 3)
                }
                mkSymbolInfo "H1: B" SymbolKind.String {
                    Uri = doc2.Id.Uri
                    Range = Range.Mk(0, 0, 0, 3)
                }
            ],
            symbols
        )

module DocSymbols =
    let fakeDoc =
        FakeDoc.Mk(
            [|
                "# E" //
                "## D"
                "### B"
                "## C"
                "# A"
            |]
        )

    [<Fact>]
    let order_noHierarchy () =
        let syms = Symbols.docSymbols false false fakeDoc

        let symNames =
            match syms with
            | First x -> x
            | _ -> failwith "Unexpected symbol type"
            |> Array.map (fun x -> x.Name)

        Assert.Equal<string>(
            [|
                "H1: E"
                "H2: D"
                "H3: B"
                "H2: C"
                "H1: A"
            |],
            symNames
        )

    [<Fact>]
    let order_Hierarchy () =
        let syms = Symbols.docSymbols true false fakeDoc

        let syms =
            match syms with
            | Second x -> x
            | _ -> failwith "Unexpected symbol type"

        let names = ResizeArray()

        let rec collect (sym: DocumentSymbol) =
            names.Add(sym.Name)
            sym.Children |> Option.defaultValue [||] |> Array.iter collect

        syms |> Array.iter collect

        Assert.Equal<string>([| "E"; "D"; "B"; "C"; "A" |], names)
