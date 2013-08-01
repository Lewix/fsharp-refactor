namespace FSharpRefactor.Tests.ProjectTests

open System.IO
open NUnit.Framework

[<TestFixture>]
type ProjectModule() =
    let files = ["file1.fs"; "file2.fs"]

    [<SetUp>]
    member this.CreateFiles () =
        let fileWriters = List.map (fun (f:string) -> new StreamWriter(f)) files
        
        fprintf fileWriters.[0] "module TestModule1 ="
        fprintf fileWriters.[0] "  let TopLevelFunction1 a = 1+a"
        fprintf fileWriters.[0] "  let TopLevelFunction2 b = 2*(TopLevelFunction1 b)"
        
        fprintf fileWriters.[1] "module TestModule2 ="
        fprintf fileWriters.[1] "   open TestModule1"
        fprintf fileWriters.[1] "   let TopLevelFunction3 a b ="
        fprintf fileWriters.[1] "       let f a = 3*a"
        fprintf fileWriters.[1] "       f(TopLevelFunction1 (TopLevelFunction2 b))"
        
        List.map (fun (f:StreamWriter) -> f.Flush()) fileWriters
        
    [<TearDown>]
    member this.DeleteFiles () =
        List.map File.Delete files
        
    