namespace FSharpRefactor.Tests.ExtractFunctionTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Refactorings.ExtractFunction

[<TestFixture>]
type ExtractFunctionAnalysisModule() =
    [<Test>]
    member this.``Can check arguments separately``() =
        Assert.IsTrue(IsValid (Some ((1,9),(1,11)),None) "let x = 1+1" "test.fs", "Valid range")
        Assert.IsFalse(IsValid (Some ((1,3),(1,11)),None) "let x = 1+1" "test.fs", "Invalid range")

    [<Test>]
    member this.``Cannot extract a function if there is no expression at expressionRange``() =
        let source = "(2+3)+(3+4)"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 0) (mkPos 1 11)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 6)
        let valid = CanExtractFunction tree inScopeTree expressionRange

        Assert.AreEqual(Invalid("No expression found at the given range"),
                        valid,
                        sprintf "Extract function validity was incorrect: %A" valid)

    [<Test>]
    member this.``Cannot extract a function if expressionRange is not contained within inScopeTree``() =
        let source = "(1+2)+(3+4)"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 0) (mkPos 1 5)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 6) (mkPos 1 11)
        let valid = CanExtractFunction tree inScopeTree expressionRange

        Assert.AreEqual(Invalid("The expression is not contained within the specified scope"),
                        valid,
                        sprintf "Extract function validity was incorrect: %A" valid)

    [<Test>]
    member this.``Cannot extract an expression which is an application of an infix expression``() =
        let source = "1+2+3"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 0) (mkPos 1 5)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 4)
        let valid = CanExtractFunction tree inScopeTree expressionRange

        Assert.AreEqual(Invalid("The expression is a partial application of an infix function"),
                        valid,
                        sprintf "Extract function validity was incorrect: %A" valid)

    [<Test>]
    member this.``Cannot extract a function with a taken name``() =
        let source = "let f a = 1 in (f 1)+(1+2)"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 15) (mkPos 1 26)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 21) (mkPos 1 26)

        ignore (Assert.Throws<RefactoringFailure>(fun () -> ignore (DoExtractFunction source tree inScopeTree expressionRange "f")))

    [<Test>]
    member this.``Can find a suitable default inScopeTree from expressionRange``() =
        let source1 = "let f a b = 5*a + b"
        let source2 = "let f a b =\n  let x = 5*a + b"
        let expressionRange1 = mkRange "test.fs" (mkPos 1 13) (mkPos 1 16)
        let expressionRange2 = mkRange "test.fs" (mkPos 1 24) (mkPos 1 27)
        let expected1 = "5*a + b"
        let expected2 = "let x = 5*a + b"

        Assert.AreEqual(expected1,
                        CodeTransforms.TextOfRange source1 (Ast.GetRange (Ast.AstNode.Expression (DefaultInScopeTree (Ast.Parse source1).Value expressionRange1).Value)).Value)
        Assert.AreEqual(expected2,
                        CodeTransforms.TextOfRange source2 (Ast.GetRange (Ast.AstNode.Expression (DefaultInScopeTree (Ast.Parse source2).Value expressionRange2).Value)).Value)

    [<Test>]
    member this.``Can find a default inScopeTree if there are no bindings in the source``() =
        let source = "1+2+3"
        let tree = (Ast.Parse source).Value
        let expressionRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 5)

        Assert.AreEqual(source,
                        CodeTransforms.TextOfRange source (Ast.GetRange (Ast.AstNode.Expression (DefaultInScopeTree tree expressionRange).Value)).Value)

[<TestFixture>]
type ExtractFunctionTransformModule() =
    [<Test>]
    member this.``Can extract an expression into a value, if it needs no arguments``() =
        let source = "1+3+4+4"
        let expected = "let a = (1+3+4+4) in a"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 0) (mkPos 1 7)) tree)

        let expressionRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 7)

        Assert.AreEqual(expected, DoExtractFunction source tree letTree expressionRange "a")

    [<Test>]
    member this.``Can disambiguate between identifiers with the same name``() = 
        let source = "let f a = (let a = 1+a in a+2)"
        let expected = "let f a = let g a = (a+2) in (let a = 1+a in (g a))"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 10) (mkPos 1 30)) tree)

        let expressionRange = mkRange "test.fs" (mkPos 1 26) (mkPos 1 29)

        Assert.AreEqual(expected, DoExtractFunction source tree letTree expressionRange "g")
    
    [<Test>]
    member this.``Can extract an expression into a function around a LetOrUse expression``() =
        let source = "let c = 1 in let a b = 1+(b+c)+4"
        let expected = "let c = 1 in let f b = ((b+c)) in let a b = 1+(f b)+4"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 13) (mkPos 1 32)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 25) (mkPos 1 30)

        Assert.AreEqual(expected, DoExtractFunction source tree letTree expressionRange "f")
        
    [<Test>]
    member this.``Can extract an expression into a function aroud a Let expression``() =
        let source = "let a b = b+b"
        let expected = "let double b = (b+b) in let a b = (double b)"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 0) (mkPos 1 13)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 10) (mkPos 1 13)

        Assert.AreEqual(expected, DoExtractFunction source tree letTree expressionRange "double")
        
    [<Test>]
    member this.``Can indent a multiline function definition before inserting it``() =
        let source = "let f a =\n    match a with\n        | Some(x) -> x\n        | None -> 0"
        let expected = "let f a =\n    let g =\n        match a with\n            | Some(x) -> x\n            | None -> 0\n    g"
        let tree = (Ast.Parse source).Value
        let matchTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 2 4) (mkPos 4 19)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 2 4) (mkPos 4 19)
        let result = DoExtractFunction source tree matchTree expressionRange "g"

        Assert.AreEqual(expected, result,
                        sprintf "The resulting string was: %s" result)

    [<Test>]
    member this.``Can turn off extract function checks``() =
        let source = "let f a = 1 in (f 1)+(1+2)"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 15) (mkPos 1 26)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 21) (mkPos 1 26)

        Assert.AreEqual("let f a = 1 in let f = ((1+2)) in (f 1)+f", RunRefactoring (ExtractFunction false inScopeTree expressionRange "f") () source)

[<TestFixture>]
type CreateFunctionModule() =
    [<Test>]
    member this.``Can get changes``() =
        let source = "let f a = 1+1"
        let expected = [(1,1),(1,13),"let f a = let g = (1+1) in g"]
        Assert.AreEqual(expected, GetChanges (((1,11),(1,14)), "g") source "test.fs")
        
    [<Test>]
    member this.``Can add a function to an expression``() =
        let expected = "let f a b =\n    a+b\n"
        let insertRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 0)
        Assert.AreEqual(expected, RunRefactoring (CreateFunction "f" ["a";"b"] "a+b" true "" insertRange) () "")

    [<Test>]
    member this.``Can add a function with multiple lines in its body to an expression``() =
        let expected = "let f a b =\n    match a,b with\n        | (a,b) -> 1\n"
        let insertRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 0)
        Assert.AreEqual(expected, RunRefactoring (CreateFunction "f" ["a";"b"] "match a,b with\n    | (a,b) -> 1" true "" insertRange) () "")
