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
