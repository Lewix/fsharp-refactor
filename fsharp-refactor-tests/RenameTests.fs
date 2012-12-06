namespace FSharpRefactor.Tests.RenameTests

open NUnit.Framework

open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Refactorings.Rename

[<TestFixture>]
type ScopeTreeModule() =
    [<Test>]
    member this.``Creates a scope tree for a simple let statement``() =
        let source = "let a = 1 in a"
        let rootNode = Ast.Parse source
        let scopeTree = makeScopeTree (rootNode.Value)

        match scopeTree with
            | [ScopeTree.Declaration(["a",_],[ScopeTree.Usage("a",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a = 1 in a' was incorrect:\n" + (sprintf "%A" scopeTree))

    [<Test>]
    member this.``Creates a scope tree for a more elaborate sequence of let statements``() =
        let source = "let a =\n  let b = 1\n  let c = 2 + b + b\n  let d = 1\n  b+c\nlet b = 3+a"
        let rootNode = Ast.Parse source
        let scopeTree = makeScopeTree (rootNode.Value)

        match scopeTree with
            | [ScopeTree.Declaration(["a",_],[ScopeTree.Usage("a",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a = 1 in a' was incorrect:\n" + (sprintf "%A" scopeTree))

[<TestFixture>]
type RenameAnalysisModule() =
    [<Test>]
    member this.``Simple renaming analysis is correct``() =
        let goodSource = "let a = 1 in a"
        let badSource = "let a = 1 in b"
        let declarationRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 5)

        Assert.IsTrue(CanRename (Ast.Parse goodSource).Value ("a", declarationRange) "b",
                      "Should be able to rename a to b")
        Assert.IsFalse(CanRename (Ast.Parse badSource).Value ("a", declarationRange) "b",
                       "Shouldn't be able to rename a to b")

    [<Test>]
    member this.``Renaming analysis with nested declaration is correct``() =
        let goodSource = "let a = 1 in let b = 2 in b"
        let badSource = "let a = 1 in let b = 2 in a"
        let declarationRange = mkRange "/home/lewis/test.fs" (mkPos 1 5) (mkPos 1 5)
 
        Assert.IsTrue(CanRename (Ast.Parse goodSource).Value ("a", declarationRange) "b",
                      "Should be able to rename a to b")
        Assert.IsFalse(CanRename (Ast.Parse badSource).Value ("a", declarationRange) "b",
                       "Shouldn't be able to rename a to b")
