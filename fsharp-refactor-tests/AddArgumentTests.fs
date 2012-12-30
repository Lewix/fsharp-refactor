namespace FSharpRefactor.Tests.AddArgumentTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Refactorings.AddArgument

[<TestFixture>]
type AddArgumentModule() =
    [<Test>]
    member this.``Can add an argument to a binding``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 15)
        let expected = "let f c a b = a+b"

        Assert.AreEqual(expected, AddArgumentToBinding source tree bindingRange "c")

    [<Test>]
    member this.``Can add an argument to a function call``() =
        let source = "f a \"b\" 3"
        let tree = (Ast.Parse source).Value
        let callRange = mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 3)
        let expected ="f \"arg\" a \"b\" 3"

        Assert.AreEqual(expected, AddArgumentToFunctionCall source tree callRange "\"arg\"")

    [<Test>]
    member this.``Can find all the App nodes calling a certain function``() =
        let source = "(let f a b c = 1 in (f 1 2 3) + ((f 2) 2) + (1 + (2 + (f 3 3 4)))) + (f 1)"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 5) (mkPos 1 16)
        let functionCalls = FindFunctionCalls source tree bindingRange "f"

        match functionCalls with
            | [Ast.AstNode.Expression(SynExpr.App(_,_,_,SynExpr.Const(SynConst.Int32(1),_),_));
               Ast.AstNode.Expression(SynExpr.App(_,_,_,SynExpr.Const(SynConst.Int32(2),_),_));
               Ast.AstNode.Expression(SynExpr.App(_,_,_,SynExpr.Const(SynConst.Int32(3),_),_))] -> ()
            | _ -> Assert.Fail("Did no get the correct function calls: " + (sprintf "%A" functionCalls))
