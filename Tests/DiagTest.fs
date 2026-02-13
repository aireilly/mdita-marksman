module Marksman.DiagTest

open Xunit

open Marksman.Diag
open Marksman.Helpers
open Marksman.Index
open Marksman.Names
open Marksman.Paths
open Marksman.Doc
open Marksman.Config
open Marksman.Folder

let entryToHuman (entry: Entry) =
    let lsp = diagToLsp entry
    lsp.Message

let diagToHuman (diag: seq<DocId * list<Entry>>) : list<string * string> =
    seq {
        for id, entries in diag do
            for e in entries do
                yield id.Path |> RootedRelPath.relPathForced |> RelPath.toSystem, entryToHuman e
    }
    |> List.ofSeq

[<Fact>]
let documentIndex_1 () =
    let doc = FakeDoc.Mk "# T1\n# T2"

    let titles =
        Doc.index >> Index.titles <| doc
        |> Array.map (fun x -> x.data.title.text)

    Assert.Equal<string>([ "T1"; "T2" ], titles)

[<Fact>]
let nonBreakingWhitespace () =
    let nbsp = "\u00a0"
    let doc = FakeDoc.Mk $"# T1\n##{nbsp}T2"

    match (checkNonBreakingWhitespace doc) with
    | [ NonBreakableWhitespace range ] ->
        Assert.Equal(1, range.Start.Line)
        Assert.Equal(1, range.End.Line)

        Assert.Equal(2, range.Start.Character)
        Assert.Equal(3, range.End.Character)
    | _ -> failwith "Expected NonBreakingWhitespace diagnostic"

[<Fact>]
let noDiagOnShortcutLinks () =
    let doc = FakeDoc.Mk([| "# H1"; "## H2"; "[shortcut]"; "[[#h42]]" |])
    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent heading 'h42'" ], diag)

[<Fact>]
let noDiagOnRealUrls () =
    let doc =
        FakeDoc.Mk([| "# H1"; "## H2"; "[](www.bad.md)"; "[](https://www.good.md)" |])

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'www.bad.md'" ], diag)

[<Fact>]
let noDiagOnNonMarkdownFiles () =
    let doc =
        FakeDoc.Mk(
            [|
                "# H1"
                "## H2"
                "[](bad.md)"
                "[](another%20bad.md)"
                "[](good/folder)"
            |]
        )

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>(
        [
            "fake.md", "Link to non-existent document 'bad.md'"
            "fake.md", "Link to non-existent document 'another bad.md'"
        ],
        diag
    )


[<Fact>]
let crossFileDiagOnBrokenWikiLinks () =
    let doc = FakeDoc.Mk([| "[[bad]]" |])

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'bad'" ], diag)

[<Fact>]
let noCrossFileDiagOnSingleFileFolders () =
    let doc =
        FakeDoc.Mk(
            [|
                "[](bad.md)" //
                "[[another-bad]]"
                "[bad-ref][bad-ref]"
            |]
        )

    let folder = Folder.singleFile doc None
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>(
        [
            "fake.md", "Link to non-existent link definition with the label 'bad-ref'"
        ],
        diag
    )

[<Fact>]
let mditaDiag_missingFrontMatter () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc = FakeDoc.Mk([| "# Title"; "Some content" |], config = mditaConfig)
    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "MDITA: Missing YAML front matter. Add a front matter block with '---' delimiters."),
        diag
    )

[<Fact>]
let mditaDiag_missingShortDescription () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc = FakeDoc.Mk([| "---"; "author: test"; "---"; "# Title" |], config = mditaConfig)
    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "MDITA: Missing short description. Add a paragraph immediately after the title heading."),
        diag
    )

[<Fact>]
let mditaDiag_invalidHeadingHierarchy () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [| "---"; "author: test"; "---"; "# Title"; "A short desc."; "#### Bad Level" |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "MDITA: Heading level 4 skips levels. Expected at most level 2."),
        diag
    )

[<Fact>]
let mditaDiag_noMditaDiagsWhenDisabled () =
    let doc = FakeDoc.Mk([| "# Title"; "Some content" |])
    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    // Should not contain any MDITA diagnostics when MDITA mode is off
    for _, msg in diag do
        Assert.False(msg.StartsWith("MDITA:"), $"Unexpected MDITA diagnostic: {msg}")

[<Fact>]
let mditaDiag_shortcutLinksWarnInMditaMode () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc = FakeDoc.Mk([| "# H1"; "[shortcut]" |], config = mditaConfig)
    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    // In MDITA mode, shortcut links should produce diagnostics
    let hasBrokenLink =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("non-existent"))

    Assert.True(hasBrokenLink, "Expected broken link diagnostic for shortcut link in MDITA mode")

[<Fact>]
let schemaDiag_taskMissingProcedure () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:task.xsd"
                "---"
                "# Install the software"
                "Some introductory text."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "Task topic should contain procedure steps (ordered list)"),
        diag
    )

[<Fact>]
let schemaDiag_taskWithOrderedListNoDiag () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:task.xsd"
                "---"
                "# Install the software"
                "Some introductory text."
                ""
                "1. First step"
                "2. Second step"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasTaskDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("procedure steps"))

    Assert.False(hasTaskDiag, "Should not warn about missing procedure when ordered list is present")

[<Fact>]
let schemaDiag_conceptWithOrderedList () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:concept.xsd"
                "---"
                "# About the software"
                "Some description."
                ""
                "1. Step one"
                "2. Step two"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "Concept topic contains ordered list; consider using task schema if these are procedure steps"),
        diag
    )

[<Fact>]
let schemaDiag_conceptWithNoOrderedListNoDiag () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:concept.xsd"
                "---"
                "# About the software"
                "Some description."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasConceptDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("ordered list"))

    Assert.False(hasConceptDiag, "Should not warn about ordered list when none is present")

[<Fact>]
let schemaDiag_referenceMissingTable () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:reference.xsd"
                "---"
                "# API Reference"
                "Some reference content without tables."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "Reference topic typically contains tables or definition lists"),
        diag
    )

[<Fact>]
let schemaDiag_referenceWithTableNoDiag () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:reference.xsd"
                "---"
                "# API Reference"
                "Some reference content."
                ""
                "| Header 1 | Header 2 |"
                "| --------- | --------- |"
                "| Cell 1 | Cell 2 |"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasRefDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("tables or definition lists"))

    Assert.False(hasRefDiag, "Should not warn about missing table when table is present")

[<Fact>]
let schemaDiag_unrecognizedSchema () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:example:custom:schema"
                "---"
                "# Some Topic"
                "Content here."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "Unrecognized $schema: 'urn:example:custom:schema'"),
        diag
    )

[<Fact>]
let schemaDiag_noSchemaNoSchemaDiags () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "author: test"
                "---"
                "# Some Topic"
                "Content here."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasSchemaDiag =
        diag
        |> List.exists (fun (_, msg) ->
            msg.Contains("$schema")
            || msg.Contains("procedure steps")
            || msg.Contains("ordered list")
            || msg.Contains("tables or definition lists")
            || msg.Contains("navigation links"))

    Assert.False(hasSchemaDiag, "Should not produce schema diagnostics when no $schema is present")

[<Fact>]
let schemaDiag_noSchemaDiagsWhenMditaDisabled () =
    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:task.xsd"
                "---"
                "# Install"
                "Content without ordered list."
            |]
        )

    let folder = FakeFolder.Mk([ doc ])
    let diag = checkFolder folder |> diagToHuman

    let hasSchemaDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("procedure steps"))

    Assert.False(hasSchemaDiag, "Schema diagnostics should only appear when MDITA is enabled")
