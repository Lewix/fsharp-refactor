namespace FSharpRefactor.Tests.RenameTests

open NUnit.Framework

open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.Rename

[<TestFixture>]
type RenameAnalysisModule() =
    [<Test>]
    member this.``Simple renaming analysis is correct``() =
        let goodSource = "let a = 1 in a"
        let badSource = "let a = 1 in b"
        let declarationRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 5)

        Assert.AreEqual(Valid,CanRename (Ast.Parse goodSource).Value ("a", declarationRange) "b",
                        "Should be able to rename a to b")
        Assert.AreEqual(Invalid("b is free in the scope of a"),CanRename (Ast.Parse badSource).Value ("a", declarationRange) "b",
                        "Shouldn't be able to rename a to b")

    [<Test>]
    member this.``Renaming analysis with nested declaration is correct``() =
        let goodSource = "let a = 1 in let b = 2 in b"
        let badSource = "let a = 1 in let b = 2 in a"
        let declarationRange = mkRange "/home/lewis/test.fs" (mkPos 1 5) (mkPos 1 5)
 
        Assert.AreEqual(Valid,CanRename (Ast.Parse goodSource).Value ("a", declarationRange) "b",
                        "Should be able to rename a to b")
        Assert.AreEqual(Invalid("a is free in the scope of a b defined in its scope"),
                        CanRename (Ast.Parse badSource).Value ("a", declarationRange) "b",
                        "Shouldn't be able to rename a to b")

[<TestFixture>]
type RenameTransformModule() =
    [<Test>]
    member this.``Can carry out renaming transformation``() =
        let source = "let a = 1 in let b = 2 in a + b + (let a = 3 in a)"
        let expected = "let c = 1 in let b = 2 in c + b + (let a = 3 in a)"
        let declarationRange = mkRange "/home/lewis/test.fs" (mkPos 1 5) (mkPos 1 5)

        Assert.AreEqual(expected, DoRename source (Ast.Parse source).Value ("a", declarationRange) "c")
 
    [<Test>]
    member this.``Can carry out another renaming transformation``() =
        let source = "let a = a in let b = 3*a + a"
        let expected = "let c = a in let b = 3*c + c"
        let declarationRange = mkRange "/home/lewis/test.fs" (mkPos 1 5) (mkPos 1 5)

        Assert.AreEqual(expected, DoRename source (Ast.Parse source).Value ("a", declarationRange) "c")

        
    [<Test>]
    member this.``Can carry out rename on a match expression``() =
        let source = "match a with (a,b) -> a"
        let expected = "match a with (c,b) -> c"
        let declarationRange = mkRange "/home/lewis/test.fs" (mkPos 1 15) (mkPos 1 15)

        Assert.AreEqual(expected, DoRename source (Ast.Parse source).Value ("a", declarationRange) "c")

        
