namespace FSharpRefactor.Tests.EngineTests

open System.Collections
open NUnit.Framework

open FSharpRefactor.Engine

[<TestFixture>]
type ASTFetcherModule() =
    [<Test>]
    member this.ThereIsAParseTree() =
        Assert.IsTrue(ASTFetcher.parseTree.IsSome)
