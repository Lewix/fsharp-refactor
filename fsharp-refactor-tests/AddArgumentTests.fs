namespace FSharpRefactor.Tests.AddArgumentTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
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
        let callRange = mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 9)
        let expected ="f a \"b\" 3 \"arg\""

        Assert.AreEqual(expected, AddArgumentToFunctionCall source tree callRange "\"arg\"")
