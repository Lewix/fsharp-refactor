namespace FSharpRefactor.Tests.ExtractFunctionTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.ExtractFunction


[<TestFixture>]
type ExtractFunctionAnalysisModule() =
    [<Test>]
    member this.``Cannot extract a function if there is no expression at expressionRange``() =
        let source = "(2+3)+(3+4)"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 11)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 6)
        let valid = CanExtractFunction source tree inScopeTree expressionRange "f"

        Assert.AreEqual(Invalid("No expression found at the given range"),
                        valid,
                        sprintf "Extract function validity was incorrect: %A" valid)

    [<Test>]
    member this.``Cannot extract a function if expressionRange is not contained within inScopeTree``() =
        let source = "(1+2)+(3+4)"
        let tree = (Ast.Parse source).Value
        let inScopeTree =
            List.head (FindNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 5)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 6) (mkPos 1 11)
        let valid = CanExtractFunction source tree inScopeTree expressionRange "f"

        Assert.AreEqual(Invalid("The expression is not contained within the specified scope"),
                        valid,
                        sprintf "Extract function validity was incorrect: %A" valid)

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

    [<Test>]
    member this.``Can find a suitable default inScopeTree from expressionRange``() =
        let source1 = "let f a b = 5*a + b"
        let source2 = "let f a b =\n  let x = 5*a + b"
        let expressionRange1 = mkRange "/home/lewis/test.fs" (mkPos 1 13) (mkPos 1 16)
        let expressionRange2 = mkRange "/home/lewis/test.fs" (mkPos 1 24) (mkPos 1 27)
        let expected1 = "5*a + b"
        let expected2 = "let x = 5*a + b"

        Assert.AreEqual(expected1,
                        CodeTransforms.TextOfRange source1 (Ast.GetRange (DefaultInScopeTree source1 (Ast.Parse source1).Value expressionRange1).Value).Value)
        Assert.AreEqual(expected2,
                        CodeTransforms.TextOfRange source2 (Ast.GetRange (DefaultInScopeTree source2 (Ast.Parse source2).Value expressionRange2).Value).Value)

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
