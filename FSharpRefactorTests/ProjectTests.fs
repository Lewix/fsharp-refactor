namespace FSharpRefactor.Tests.ProjectTests

open System.IO
open NUnit.Framework
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

[<TestFixture>]
type ModuleScopeTreeModule() =
    let files = ["file1.fs"; "file2.fs"; "test.fs"]
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
    member this.``Can get a list of modules from a piece of source code``() =
        let filename = Path.GetFullPath "test.fs"
        let source =
            String.concat "\n" ["namespace Test";
                                "module M1 =";
                                "  let D1 = 1";
                                "  let D2 = 2";
                                "module M2 =";
                                "  let D2 = 2";
                                "  module M3 =";
                                "    let D4 = 4"]
        let modules = Modules.GetModules (new Project(source, filename))
        let expected =
            [{
                fullName = "Test", ["M1"];
                declarations = ["D1", mkRange filename (mkPos 3 6) (mkPos 3 8); "D2", mkRange filename (mkPos 4 6) (mkPos 4 8)];
                nestedModules = [];
                filename = filename
            };
            {
                fullName = "Test", ["M2"];
                declarations = ["D2", mkRange filename (mkPos 6 6) (mkPos 6 8)];
                nestedModules = 
                    [{
                        fullName = "Test", ["M2";"M3"];
                        declarations = ["D4", mkRange filename (mkPos 8 8) (mkPos 8 10)];
                        nestedModules = [];
                        filename = filename
                    }];
                filename = filename
            }]
        
        Assert.AreEqual(expected, modules, sprintf "%A" modules)
        
    [<Test>]
    member this.``Can get a list of modules from an entire project``() =
        let files = [files.[0]; files.[1]]
        let fullFilenames = List.map Path.GetFullPath files
        let project = new Project(files.[0], List.zip files [None; None] |> List.toArray)
        let modules = Modules.GetModules project
        let expected =
            [{
                fullName = "Test", ["TestModule1"];
                declarations = ["TopLevelFunction1", mkRange fullFilenames.[0] (mkPos 3 6) (mkPos 3 23);
                                "TopLevelFunction2", mkRange fullFilenames.[0] (mkPos 4 6) (mkPos 4 23)];
                nestedModules = [];
                filename = fullFilenames.[0]
            };
            {
                fullName = "Test", ["TestModule2"];
                declarations = ["TopLevelFunction1", mkRange fullFilenames.[0] (mkPos 6 6) (mkPos 6 23)];
                nestedModules = [];
                filename = fullFilenames.[0]
            };
            {
                fullName = "Test", ["TestModule3"];
                declarations = ["TopLevelFunction3", mkRange fullFilenames.[1] (mkPos 4 7) (mkPos 4 24)];
                nestedModules = [];
                filename = fullFilenames.[1]
            }]
        
        Assert.AreEqual(expected, modules, sprintf "%A" modules)