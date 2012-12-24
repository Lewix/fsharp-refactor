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
    member this.``Can extract an expression into a function around a LetOrUse expression``() =
        let source = "let c = 1 in let a b = 1+(b+c)+4"
        let expected = "let c = 1 in let f b = b+c in let a b = 1+(f b)+4"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (findNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 13) (mkPos 1 32)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 25) (mkPos 1 30)

        Assert.AreEqual(expected, DoExtractFunction source letTree expressionRange "f")
        
    [<Test>]
    member this.``Can extract an expression into a function aroud a Let expression``() =
        let source = "let a b = b+b"
        let expected = "let double b = b+b in let a b = (double b)"
        let tree = (Ast.Parse source).Value
        let letTree =
            List.head (findNodesWithRange (mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 13)) tree)
        let expressionRange = mkRange "/home/lewis/test.fs" (mkPos 1 10) (mkPos 1 13)

        Assert.AreEqual(expected, DoExtractFunction source letTree expressionRange "double")
        

[<TestFixture>]
type CreateFunctionModule() =
    [<Test>]
    member this.``Can add a function to an expression``() =
        let source = "let a = 1"
        let expected = "let f a b = a+b in "
        let tree = (Ast.Parse source).Value
        let oneRange = mkRange "/home/lewis/test.fs" (mkPos 1 8) (mkPos 1 9)
        let expression = findExpressionAtRange oneRange tree

        Assert.AreEqual(expected, CreateFunction source expression "f" ["a";"b"] "a+b" false)
