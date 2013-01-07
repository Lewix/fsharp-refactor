namespace FSharpRefactor.Tests.ExtractFunctionTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.ExtractFunction


[<TestFixture>]
type ExtractFunctionAnalysisModule() =
    [<Test>]
    member this.``Cannot extract a function with a taken name``() =
        let source = "let f a = 1 in (f 1)+(1+2)"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 15) (mkPos 1 26)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 21) (mkPos 1 26)

        ignore (Assert.Throws<RefactoringFailure>(fun () -> ignore (DoExtractFunction source tree inScopeTree expressionRange "f")))

    [<Test>]
    member this.``Can find an unused name``() =
        let source = "let f a b c = 1\nlet g = 3"
        let tree = (Ast.Parse source).Value
        
        Assert.IsFalse(Set.contains (findUnusedName tree) (Set ["f";"a";"b";"c";"g"]))


[<TestFixture>]
type ExtractFunctionTransformModule() =
    [<Test>]
    member this.``Can extract an expression into a value, if it needs no arguments``() =
        let source = "1+3+4+4"
        let expected = "let a = 1+3+4+4 in (a)"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 7)) tree)

        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 7)

        Assert.AreEqual(expected, DoExtractFunction source tree letTree expressionRange "a")
    
    [<Test>]
    member this.``Can extract an expression into a function around a LetOrUse expression``() =
        let source = "let c = 1 in let a b = 1+(b+c)+4"
        let expected = "let c = 1 in let f b = b+c in let a b = 1+(f b)+4"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 13) (mkPos 1 32)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 25) (mkPos 1 30)

        Assert.AreEqual(expected, DoExtractFunction source tree letTree expressionRange "f")
        
    [<Test>]
    member this.``Can extract an expression into a function aroud a Let expression``() =
        let source = "let a b = b+b"
        let expected = "let double b = b+b in let a b = (double b)"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 13)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 10) (mkPos 1 13)

        Assert.AreEqual(expected, DoExtractFunction source tree letTree expressionRange "double")
        

[<TestFixture>]
type CreateFunctionModule() =
    [<Test>]
    member this.``Can add a function to an expression``() =
        let source = "let a = 1"
        let expected = "let f a b = a+b in "
        let tree = (Ast.Parse source).Value
        let oneRange = mkRange "/home/lewis/test.fs" (mkPos 1 8) (mkPos 1 9)
        let expression = FindExpressionAtRange oneRange tree

        Assert.AreEqual(expected, CreateFunction source expression "f" ["a";"b"] "a+b" false)
