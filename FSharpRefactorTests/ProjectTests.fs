namespace FSharpRefactor.Tests.ProjectTests

open System.IO
open NUnit.Framework
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis
open FSharpRefactor.Engine.ModuleScopeTree

[<TestFixture>]
type ModuleScopeTreeModule() =
    let files = ["file1.fs"; "file2.fs"]
    let incorrectScopeTrees source scopeTrees =
        sprintf "ModuleScopeTrees for '%s' were incorrect:\n %A" source scopeTrees
    let singleFileProject source =
        new Project(source, "test.fs")

    [<SetUp>]
    member this.CreateFiles () =
        let fileWriters = List.map (fun (f:string) -> new StreamWriter(f)) files
        
        fprintfn fileWriters.[0] "namespace Test"
        fprintfn fileWriters.[0] "module TestModule1 ="
        fprintfn fileWriters.[0] "  let TopLevelFunction1 a = 1+a"
        fprintfn fileWriters.[0] "  let TopLevelFunction2 b = 2*(TopLevelFunction1 b)"
        fprintfn fileWriters.[0] "module TestModule2 ="
        fprintfn fileWriters.[0] "  let TopLevelFunction1 c = 3"
        
        fprintfn fileWriters.[1] "namespace Test"
        fprintfn fileWriters.[1] "module TestModule3 ="
        fprintfn fileWriters.[1] "   open TestModule1"
        fprintfn fileWriters.[1] "   let TopLevelFunction3 a b ="
        fprintfn fileWriters.[1] "       let f a = 3*a"
        fprintfn fileWriters.[1] "       f(TopLevelFunction1 (TopLevelFunction2 b))"
        
        List.map (fun (f:StreamWriter) -> f.Flush()) fileWriters |> ignore
        List.map (fun (f:StreamWriter) -> f.Close()) fileWriters |> ignore
        
    [<TearDown>]
    member this.DeleteFiles () =
        List.map File.Delete files
        |> ignore
        
    [<Test>]
    member this.``Can create the scope trees for a single module declaration``() =
        let source = "module TestModule1\n  let TopLevelFunction1 a = 1+a\n  let TopLevelFunction2 = 2*(TopLevelFunction1b)"
        let moduleScopeTrees = makeModuleScopeTrees (singleFileProject source)
        
        match moduleScopeTrees with
            | [Declaration((("TestModule1", r), ["TestModule1.TopLevelFunction1",_;"TestModule1.TopLevelFunction2",_]),[])] when r = mkRange "test.fs" (mkPos 1 0) (mkPos 3 48) -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source moduleScopeTrees)

    [<Test>]
    member this.``Can create the scope trees for a module declaration with nested modules``() =
        let source = "module TestModule1\n  let TopLevelValue1 = 1\n  module TestModule2 =\n    let TopLevelValue2 = 2"
        let moduleScopeTrees = makeModuleScopeTrees (singleFileProject source)
        
        match moduleScopeTrees with
            | [Declaration((("TestModule1", _), ["TestModule1.TopLevelValue1",_;"TestModule1.TestModule2.TopLevelValue2",_]),  [])] ->  ()
            | _ -> Assert.Fail(incorrectScopeTrees source moduleScopeTrees)
            
    [<Test>]
    member this.``Can create the scope trees for namespaces with nested modules``() =
        let source = "namespace Test1\n\nmodule TestModule1 =\n  let TopLevelValue1 = 1\n\nnamespace Test2\n\nmodule TestModule2 =\n  let TopLevelValue2 = 2"
        let moduleScopeTrees = makeModuleScopeTrees (singleFileProject source)
        
        match moduleScopeTrees with
            | [Declaration((("Test1.TestModule1", _), ["Test1.TestModule1.TopLevelValue1",_]),
                [Declaration((("Test2.TestModule2", _), ["Test2.TestModule2.TopLevelValue2",_]),[])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source moduleScopeTrees)
            
    [<Test>]
    member this.``Can create the scope trees for a namespace with nested modules``() =
        let source = "namespace Test\n\nmodule TestModule1 = \n  let TopLevelValue1 = 1\n\nmodule TestModule2 =\n  let TopLevelValue2 = 2"
        let moduleScopeTrees = makeModuleScopeTrees (singleFileProject source)
        
        match moduleScopeTrees with
            | [Declaration((("Test.TestModule1",_), ["Test.TestModule1.TopLevelValue1",_]),
                [Declaration((("Test.TestModule2",_), ["Test.TestModule2.TopLevelValue2",_]),[])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source moduleScopeTrees)
            
    [<Test>]
    member this.``Can create the scope trees for code with a fully qualified module usage``() =
        let source = "namespace Test\n\nmodule TestModule1 =\n  let TopLevelValue = 1\n\nmodule TestModule2 =\n  let TopLevelValue = Test.TestModule1.TopLevelValue"
        let moduleScopeTrees = makeModuleScopeTrees (singleFileProject source)
        
        match moduleScopeTrees with
            | [Declaration((("Test.TestModule1",_),["Test.TestModule1.TopLevelValue",_]),
                [Declaration((("Test.TestModule2",_),["Test.TestModule2.TopLevelValue",_]),[]);
                 Usage(("Test.TestModule1.TopLevelValue",_))])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source moduleScopeTrees)
            
    [<Test>]
    member this.``Can create the declarations in the scope trees for modules over two files``() =
        let project = new Project(files.[0], List.zip files (List.map (fun f -> Some (File.ReadAllText f)) files) |> List.toArray)
        let moduleScopeTrees = makeModuleScopeTrees project
                
        match moduleScopeTrees with
            | [Declaration((("Test.TestModule1",_), ["Test.TestModule1.TopLevelFunction1",_; "Test.TestModule1.TopLevelFunction2",_]),
                [Declaration((("Test.TestModule2",_), ["Test.TestModule2.TopLevelFunction1",_]),
                    [Declaration((("Test.TestModule3",_), ["Test.TestModule3.TopLevelFunction3",_]),[])])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees project.CurrentFileContents moduleScopeTrees)