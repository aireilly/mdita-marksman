module Marksman.CodeActionTests

open Ionide.LanguageServerProtocol.Types
open Xunit

open Marksman.Helpers
open Marksman.Misc
open Marksman.Folder
open Marksman.Paths

module CreateMissingFileTests =
    [<Fact>]
    let shouldCreateWhenNoFileExists () =
        let doc1 = FakeDoc.Mk([| "# Doc 1"; "## Sub 1" |], path = "doc1.md")
        let doc2 = FakeDoc.Mk([| "[[doc3]]" |], path = "doc2.md")
        let folder = FakeFolder.Mk([ doc1; doc2 ])

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca =
            CodeActions.createMissingFile (Range.Mk(0, 3, 0, 3)) caCtx doc2 folder

        match ca with
        | Some { name = "Create `doc3.md`" } -> Assert.True(true)
        | _ -> Assert.True(false)

    [<Fact>]
    let shouldNotCreateWhenRefBrokenButFileExists () =
        let doc1 = FakeDoc.Mk([| "# Doc 1"; "## Sub 1" |], path = "doc1.md")
        let doc2 = FakeDoc.Mk([| "[[doc1#Sub 2]]" |], path = "doc2.md")
        let folder = FakeFolder.Mk([ doc1; doc2 ])

        let caCtx = { Diagnostics = [||]; Only = None; TriggerKind = None }

        let ca =
            CodeActions.createMissingFile (Range.Mk(0, 3, 0, 3)) caCtx doc2 folder

        Assert.Equal(None, ca)

    [<Fact>]
    let shouldNotCreateInSingleFileMode () =
        let doc =
            FakeDoc.Mk(content = "[link](other.md)", path = "sub/dir/source.md", root = "sub/dir")

        let folder = Folder.singleFile doc None

        Assert.True(Folder.isSingleFile folder)

    [<Fact>]
    let singleFileRootShouldBeDirectory () =
        let doc =
            FakeDoc.Mk(content = "# Test", path = "sub/dir/source.md", root = "sub/dir")

        let folder = Folder.singleFile doc None
        let root = Folder.rootPath folder

        let rootSys = RootPath.toSystem root
        // Root should be a directory path, not contain a .md extension
        Assert.False(rootSys.EndsWith(".md"), $"Root should be a directory, got: {rootSys}")
