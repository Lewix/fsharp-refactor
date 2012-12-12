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
                | Some(Ast.AstNode.ModuleDeclaration(_)::_) -> true
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
