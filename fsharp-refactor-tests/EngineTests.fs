namespace FSharpRefactor.Tests.EngineTests

open System
open NUnit.Framework
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine

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
                                               _,_,_,_) -> binding.RangeOfBindingAndRhs
                        | _ -> raise (new Exception("Something is wrong with Parse"))
                | _ -> raise (new Exception("Something is wrong with Parse"))
        Assert.AreEqual("a = 1", ASTFetcher.TextOfRange source range)
