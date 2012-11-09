namespace FSharpRefactor.Tests.EngineTests

open System
open NUnit.Framework
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast

[<TestFixture>]
type ASTFetcherModule() =
    [<Test>]
    member this.``Parsing some simple code is successful``() =
        let expected t =
            match t with
                | Some(ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,_,_))) -> true
                | _ -> false
        Assert.IsTrue(expected (ASTFetcher.Parse "let a = 1"))

    [<Test>]
    member this.``Can calculate the height of a parse tree``() =
        let tree = (ASTFetcher.Parse "let a = 1").Value
        // Module + let + binding + max(1 (for a), 1 (for 1)) = 4
        let expected = 4
        Assert.AreEqual(expected, ASTFetcher.Height tree)

    [<Test>]
    member this.``Can deduce the text corresponding to some range in a let statement``() =
        let source = "let a = 1"
        let tree = (ASTFetcher.Parse source).Value
        let range =
            match tree with
                | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,namespaces,_)) ->
                    match Seq.head(namespaces) with
                        | SynModuleOrNamespace(_,_,
                                               SynModuleDecl.Let(_,binding::_,_)::_,
                                               _,_,_,_) -> Some(binding.RangeOfBindingAndRhs)
                        | _ -> None
                | _ -> None
        Assert.IsTrue(range.IsSome, "Did not get the expected tree from parse")
        Assert.AreEqual("a = 1", ASTFetcher.TextOfRange source range.Value)

    [<Test>]
    member this.``Active patterns can be used to get the children of some nodes``() =
        let source = "let a = 1"
        let tree =
            match ASTFetcher.Parse source with
                | Some(ParsedInput.ImplFile(t)) -> Some(t)
                | _ -> None
        Assert.IsTrue(tree.IsSome, "Did not get the expected tree from parse")
        let ns =
            match tree.Value with
                | ParsedImplFileInput(_,_,_,_,_,ns,_) -> Seq.head(ns)
        let expected =
            match ns with
                | SynModuleOrNamespace(_,_,lets,_,_,_,_) -> Some(lets)
        let children =
            match ns with
                | Ast.ModuleOrNamespaceChildren(c) -> Some(c)
                | _ -> None
                
        Assert.AreEqual(expected, children)

[<TestFixture>]
type AstModule() =
    [<Test>]
    member this.``Can instantiate a AstNode from the result of parsing source code``() =
        let source = "let a = 1"
        let rootNode = Ast.MakeAstNode (ASTFetcher.Parse source)
        let expectNamespace node =
            match node with
                | Some(Ast.AstNode.ModuleOrNamespace(_)) -> true
                | _ -> false
        Assert.IsTrue(expectNamespace rootNode)

        let expectLet (node : Ast.AstNode option) =
            match Ast.GetChildren(node.Value) with
                | Some(Ast.AstNode.Binding(_)) -> true
                | _ -> false
        Assert.IsTrue(expectLet rootNode)
