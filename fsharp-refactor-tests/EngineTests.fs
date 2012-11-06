namespace FSharpRefactor.Tests.EngineTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine

[<TestFixture>]
type ASTFetcherModule() =
    [<Test>]
    member this.``There is a parse tree``() =
        Assert.IsTrue(ASTFetcher.parseTree.IsSome)

    [<Test>]
    member this.``Parsing some simple code is successful``() =
        let expected t =
            match t with
                | Some(ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,_,_))) -> true
                | _ -> false
        Assert.IsTrue(expected (ASTFetcher.Parse "let a = 1"))
    
