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
    let noMditaConfig = { Config.Default with coreMditaEnable = Some false }
    let doc = FakeDoc.Mk([| "# H1"; "## H2"; "[shortcut]"; "[[#h42]]" |], config = noMditaConfig)
    let folder = FakeFolder.Mk([ doc ], config = noMditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent heading 'h42'" ], diag)

[<Fact>]
let noDiagOnRealUrls () =
    let noMditaConfig = { Config.Default with coreMditaEnable = Some false }
    let doc =
        FakeDoc.Mk([| "# H1"; "## H2"; "[](www.bad.md)"; "[](https://www.good.md)" |], config = noMditaConfig)

    let folder = FakeFolder.Mk([ doc ], config = noMditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'www.bad.md'" ], diag)

[<Fact>]
let noDiagOnNonMarkdownFiles () =
    let noMditaConfig = { Config.Default with coreMditaEnable = Some false }
    let doc =
        FakeDoc.Mk(
            [|
                "# H1"
                "## H2"
                "[](bad.md)"
                "[](another%20bad.md)"
                "[](good/folder)"
            |],
            config = noMditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = noMditaConfig)
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
    let noMditaConfig = { Config.Default with coreMditaEnable = Some false }
    let doc = FakeDoc.Mk([| "[[bad]]" |], config = noMditaConfig)

    let folder = FakeFolder.Mk([ doc ], config = noMditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Equal<string * string>([ "fake.md", "Link to non-existent document 'bad'" ], diag)

[<Fact>]
let noCrossFileDiagOnSingleFileFolders () =
    let noMditaConfig = { Config.Default with coreMditaEnable = Some false }
    let doc =
        FakeDoc.Mk(
            [|
                "[](bad.md)" //
                "[[another-bad]]"
                "[bad-ref][bad-ref]"
            |],
            config = noMditaConfig
        )

    let folder = Folder.singleFile doc (Some noMditaConfig)
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
    let noMditaConfig = { Config.Default with coreMditaEnable = Some false }
    let doc = FakeDoc.Mk([| "# Title"; "Some content" |], config = noMditaConfig)
    let folder = FakeFolder.Mk([ doc ], config = noMditaConfig)
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
    let noMditaConfig = { Config.Default with coreMditaEnable = Some false }
    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:dita:xsd:task.xsd"
                "---"
                "# Install"
                "Content without ordered list."
            |],
            config = noMditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = noMditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasSchemaDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("procedure steps"))

    Assert.False(hasSchemaDiag, "Schema diagnostics should only appear when MDITA is enabled")

// --- Extended Profile feature detection tests ---

[<Fact>]
let extendedProfile_coreWithDefinitionList () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:core:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "Term 1"
                ":   Definition 1"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "'Definition lists' requires MDITA Extended Profile; document uses Core Profile"),
        diag
    )

[<Fact>]
let extendedProfile_coreWithNoExtendedFeatures () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:core:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "Some plain content."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasExtendedDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("Extended Profile"))

    Assert.False(hasExtendedDiag, "Should not warn about extended features when none are used")

[<Fact>]
let extendedProfile_extendedWithDefinitionList () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "Term 1"
                ":   Definition 1"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasExtendedDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("Extended Profile"))

    Assert.False(hasExtendedDiag, "Should not warn about extended features in extended profile docs")

[<Fact>]
let extendedProfile_ditaSchemaWithDefinitionList () =
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
                "# Title"
                "Short description."
                ""
                "Term 1"
                ":   Definition 1"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasExtendedDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("Extended Profile"))

    Assert.False(hasExtendedDiag, "DITA schemas should not trigger profile gating")

// --- Footnote validation tests ---

[<Fact>]
let footnote_refWithoutDef () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "Text with a footnote[^1]."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "Footnote reference '[^1]' has no matching definition"),
        diag
    )

[<Fact>]
let footnote_defWithoutRef () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "[^1]: Orphaned footnote definition"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "Footnote definition '[^1]' is not referenced"),
        diag
    )

[<Fact>]
let footnote_matchedRefAndDef () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "Text with a footnote[^1]."
                ""
                "[^1]: This is the footnote"
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasFootnoteDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("Footnote"))

    Assert.False(hasFootnoteDiag, "Should not produce footnote diagnostics when ref and def match")

// --- Admonition validation tests ---

[<Fact>]
let admonition_validType () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "!!! note"
                "    This is a note."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasAdmonitionDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("admonition"))

    Assert.False(hasAdmonitionDiag, "Should not warn about valid admonition type 'note'")

[<Fact>]
let admonition_invalidType () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "!!! invalid"
                "    This is an invalid admonition."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    Assert.Contains(
        ("fake.md", "Unknown admonition type 'invalid'; DITA supports: note, tip, warning, caution, danger, attention, important, notice, remember, restriction, trouble"),
        diag
    )

[<Fact>]
let admonition_warningType () =
    let mditaConfig = {
        Config.Default with
            coreMditaEnable = Some true
    }

    let doc =
        FakeDoc.Mk(
            [|
                "---"
                "$schema: urn:oasis:names:tc:mdita:extended:xsd:topic.xsd"
                "---"
                "# Title"
                "Short description."
                ""
                "!!! warning"
                "    This is a warning."
            |],
            config = mditaConfig
        )

    let folder = FakeFolder.Mk([ doc ], config = mditaConfig)
    let diag = checkFolder folder |> diagToHuman

    let hasAdmonitionDiag =
        diag
        |> List.exists (fun (_, msg) -> msg.Contains("admonition"))

    Assert.False(hasAdmonitionDiag, "Should not warn about valid admonition type 'warning'")
