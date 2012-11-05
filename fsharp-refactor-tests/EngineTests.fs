namespace FSharpRefactor.Tests.EngineTests

open System
open NUnit.Framework

open FSharpRefactor.Engine

[<TestFixture>]
type ASTFetcherModule() =
    [<Test>]
    member this.RunsWithoutErrors() =
        let result = ASTFetcher.parseTree
        ()
