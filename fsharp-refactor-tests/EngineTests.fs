namespace FSharpRefactor.Tests.EngineTests

open System
open NUnit.Framework
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.CodeAnalysis


[<TestFixture>]
type AstModule() =
    [<Test>]
    member this.``Can instantiate a AstNode from the result of parsing source code``() =
        let source = "let a = 1"
        let rootNode = Ast.Parse source
        let expectNamespace node =
            match node with
                | Some(Ast.AstNode.File(_)) -> true
                | _ -> false
        Assert.IsTrue(expectNamespace rootNode, "Should get a namespace or module from MakeAstNode")

        let binding (node : Ast.AstNode option) =
            Ast.GetChildren(List.head(Ast.GetChildren(node.Value).Value))
        let expectModule (node : Ast.AstNode option) =
            match binding node with
                | Some(Ast.AstNode.Module(_)::_) -> true
                | _ -> false
        Assert.IsTrue(expectModule rootNode)

[<TestFixture>]
type CodeTransformsModule() =
    [<Test>]
    member this.``Can change the text corresponding to an ast node``() =
        let source = "let a = 1\n\n"
        let tree = (Ast.Parse source).Value
        let a = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.Head).Value.Head).Value.Head
        let expected = "let b = 1\n\n"

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [((Ast.GetRange a).Value,"b")])

    [<Test>]
    member this.``Can change the text for two ast nodes``() =
        let source = "\nlet a = 1\nlet b = 2"
        let tree = (Ast.Parse source).Value
        let a = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.Head).Value.Head).Value.Head
        let b = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.[1]).Value.Head).Value.Head
        let expected = "\nlet b = 1\nlet a2 = 2"

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [((Ast.GetRange a).Value,"b");((Ast.GetRange b).Value,"a2")])

    [<Test>]
    member this.``Can add a child to a node``() =
        let source = "\nlet a = \n  let c = 3\n  3"
        let tree = (Ast.Parse source).Value
        let letNode = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.Head).Value.Head
        let expected = "\nlet a = \n  let b = 2\n  let c = 3\n  3"
        let actual = CodeTransforms.AddChild source letNode 1 "let b = 2\n  "
        Assert.AreEqual(expected, actual)

//TODO: test TextOfRange

[<TestFixture>]
type CodeAnalysisModule() =
    member this.parseAndRun source f =
        let tree = (Ast.Parse source).Value
        f tree

    [<Test>]
    member this.``Can count value declarations``() =
        let source = "let a = 1\n  let b = 2\nlet c = 3"
        let expected = 3
        let actual = this.parseAndRun source CodeAnalysis.CountDeclarations
        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.``Can count value usages``() =
        let source = "let a = 1\n  let b = 2\na+b\nlet c = 3"
        let expected = 2
        let actual = this.parseAndRun source CodeAnalysis.CountUsages
        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.``Can count value declarations and usages for slightly more complicated code``() =
        let source = "let CountDeclarations t = match t with \n  | (0,a) -> a\n  | (a,b) -> a+b"
        Assert.AreEqual(4, this.parseAndRun source CodeAnalysis.CountUsages, "Counting usages")
        Assert.AreEqual(4, this.parseAndRun source CodeAnalysis.CountDeclarations, "Counting declarations")
