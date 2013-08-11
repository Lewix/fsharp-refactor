namespace FSharpRefactor.Tests.RenameTests

open System.IO
open NUnit.Framework

open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Refactorings.Rename

[<TestFixture>]
type RenameAnalysisModule() =

    [<Test>]
    member this.``Can check arguments individually``() =
        Assert.IsTrue(IsValid (Some(1,5), None) (new Project("let f a = 1", "test.fs")), "Valid position")
        Assert.IsFalse(IsValid (Some(1,1), None) (new Project("let f a = 1", "test.fs")), "Invalid position")
        Assert.IsTrue(IsValid (Some(1,7), Some "c") (new Project("let f a b = 1", "test.fs")), "Valid position and name")
        Assert.IsFalse(IsValid (Some(1,7), Some "b") (new Project("let f a b = 1", "test.fs")), "Invalid position and name")
        Assert.IsFalse(IsValid (Some(1,1), Some "a") (new Project("let f a b = 1", "test.fs")), "Invalid position and name")
        Assert.IsTrue(IsValid (Some(1,7), Some "a") (new Project("let f a b = 1", "test.fs")), "Pointless rename")
        Assert.IsTrue(IsValid (None, Some "b") (new Project("let f a b = 1", "test.fs")), "Valid name")

        //TODO: invalid name

    [<Test>]
    member this.``Simple renaming analysis is correct``() =
        let goodSource = "let a = 1 in a"
        let badSource = "let a = 1 in b"

        Assert.AreEqual(true,IsValid (Some (1,5), Some "b") (new Project(goodSource, "test.fs")),
                        "Should be able to rename a to b")
        Assert.AreEqual(Some("b is free in the scope of a"),GetErrorMessage (Some (1,5), Some "b") (new Project(badSource, "test.fs")),
                        "Shouldn't be able to rename a to b")

    [<Test>]
    member this.``Renaming analysis with nested declaration is correct``() =
        let goodSource = "let a = 1 in let b = 2 in b"
        let badSource = "let a = 1 in let b = 2 in a"
 
        Assert.AreEqual(true,IsValid (Some (1,5), Some "b") (new Project(goodSource, "test.fs")),
                        "Should be able to rename a to b")
        Assert.AreEqual(Some("a is free in the scope of a b defined in its scope"),
                        GetErrorMessage (Some (1,5), Some "b") (new Project(badSource, "test.fs")),
                        "Shouldn't be able to rename a to b")

    [<Test>]
    member this.``Cannot rename to a name already bound in the same pattern``() =
        let source = "let f a b c = 1"

        Assert.AreEqual(Some("b is already declared in that pattern"),
                        GetErrorMessage (Some (1,7), Some "b") (new Project(source, "test.fs")))

[<TestFixture>]
type RenameTransformModule() =
    let parse (source:string) (filename:string) =
        Ast.Parse (new Project(source, filename)) filename

    let DoRename source (tree: Ast.AstNode) declarationIdentifier (newName : string) =
        RunRefactoring (Rename newName) declarationIdentifier source
        
    let mkRange filename startPos endPos = mkRange (Path.GetFullPath filename) startPos endPos
    let files = ["test.fs"]

    [<SetUp>]
    member this.CreateFiles () =
        List.map (fun (f:string) -> new StreamWriter(f)) files
        |> List.map (fun (s:StreamWriter) -> s.Close())
        |> ignore
    [<TearDown>]
    member this.DeleteFiles () =
        List.map File.Delete files
        |> ignore

    [<Test>]
    member this.``Can get changes``() =
        let source = "let f x = x+x"
        let expected = "let f y = y+y"
        Assert.AreEqual(expected, (Transform ((1,7), "y") (new Project(source, "test.fs"))))
        
    [<Test>]
    member this.``Can carry out renaming transformation``() =
        let source = "let a = 1 in let b = 2 in a + b + (let a = 3 in a)"
        let expected = "let c = 1 in let b = 2 in c + b + (let a = 3 in a)"
        let declarationRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 5)

        Assert.AreEqual(expected, DoRename (new Project(source, "test.fs")) (parse source "test.fs").Value ("a", declarationRange) "c")
 
    [<Test>]
    member this.``Can carry out another renaming transformation``() =
        let source = "let a = a in let b = 3*a + a in ()"
        let expected = "let c = a in let b = 3*c + c"
        let declarationRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 5)

        Assert.AreEqual(expected, DoRename (new Project(source, "test.fs")) (parse source "test.fs").Value ("a", declarationRange) "c")

    [<Test>]
    member this.``Can carry out rename on a match expression``() =
        let source = "match a with (a,b) -> a"
        let expected = "match a with (c,b) -> c"
        let declarationRange = mkRange "test.fs" (mkPos 1 14) (mkPos 1 15)

        Assert.AreEqual(expected, DoRename (new Project(source, "test.fs")) (parse source "test.fs").Value ("a", declarationRange) "c")
        
    [<Test>]
    member this.``Can rename a function and its arguments``() =
        let source = "let f a b = a+b in f 1 2"
        let expected = "let g a b = a+b in g 1 2"
        let declarationRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 5)

        Assert.AreEqual(expected, DoRename (new Project(source, "test.fs")) (parse source "test.fs").Value ("f", declarationRange) "g")

        let source = "let f a b = a+b in f 1 2"
        let expected = "let f c b = c+b in f 1 2"
        let declarationRange = mkRange "test.fs" (mkPos 1 6) (mkPos 1 7)
         
        Assert.AreEqual(expected, DoRename (new Project(source, "test.fs")) (parse source "test.fs").Value ("a", declarationRange) "c")

    [<Test>]
    member this.``Can rename a nested identifier``() =
        let source = "let a = 1 in let b = 1 in b"
        let expected = "let a = 1 in let c = 1 in c"
        let declarationRange = mkRange "test.fs" (mkPos 1 17) (mkPos 1 18)

        Assert.AreEqual(expected, DoRename (new Project(source, "test.fs")) (parse source "test.fs").Value ("b", declarationRange) "c")
