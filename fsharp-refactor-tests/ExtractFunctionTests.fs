namespace FSharpRefactor.Tests.ExtractFunctionTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Refactorings.ExtractFunction

[<TestFixture>]
type ExtractFunctionTransformModule() =
    [<Test>]
    member this.``Can find the AstNode.Expression corresponding to a range``() =
        let source = "let a = 1+(2+3)+4"
        let tree = (Ast.Parse source).Value
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 10) (mkPos 1 15)
        let expression = findExpressionAtRange expressionRange tree

        match expression with
            | Ast.AstNode.Expression(SynExpr.Paren(SynExpr.App(_,_,SynExpr.App(_,_,_,SynExpr.Const(_,_),_),SynExpr.Const(_,_),_),_,_,_)) -> ()
            | _ -> Assert.Fail("The AstNode was not the one for (2+3): " + (sprintf "%A" expression))

    [<Test>]
    member this.``Can extract an expression into a function``() =
        let source = "let a b = 1+(b+3)+4"
        let expected = "let f b = b+3 in let a b = 1+(f b)+4"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (findNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 19)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 12) (mkPos 1 17)

        Assert.AreEqual(expected, DoExtractFunction source letTree expressionRange "f")
        

[<TestFixture>]
type CreateFunctionModule() =
    [<Test>]
    member this.``Can add a function to an expression``() =
        let source = "let a = 1"
        let expected = "let a = let f a b = a+b in 1"
        let tree = (Ast.Parse source).Value
        let oneRange = mkRange "/home/lewis/test.fs" (mkPos 1 8) (mkPos 1 9)
        let expression = findExpressionAtRange oneRange tree

        Assert.AreEqual(expected, CreateFunction source expression "f" ["a";"b"] "a+b" false)
