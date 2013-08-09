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
    member this.``Can create the entire scope trees for modules over two files``() =
        let files = [files.[0]; files.[1]]
        let project = new Project(files.[0], List.zip files [None; None] |> List.toArray)
        let moduleScopeTrees = makeModuleScopeTrees project
        
        match moduleScopeTrees with
            | [Declaration((("Test.TestModule1",_), ["Test.TestModule1.TopLevelFunction1",_; "Test.TestModule1.TopLevelFunction2",_]),
                [Declaration((("Test.TestModule2",_), ["Test.TestModule2.TopLevelFunction1",_]),
                    [Declaration((("Test.TestModule3",_), ["Test.TestModule3.TopLevelFunction3",_]),[]);
                     Usage("Test.TestModule1.TopLevelFunction1",_);
                     Usage("Test.TestModule1.TopLevelFunction2",_)])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees project.CurrentFileContents moduleScopeTrees)
            
    [<Test>]
    member this.``Can create the usages in the scope trees for opened modules``() =
        let source =
            String.concat "\n" ["namespace Test";
                                "module TestModule1 =";
                                "  let TopLevelValue1 = 1";
                                "  let TopLevelValue2 = 2";
                                "  let TopLevelFunction3 a = 2*a";
                                "module TestModule2 =";
                                "  open TestModule1";
                                "  let f = TestModule1.TopLevelValue2 + (TopLevelFunction3 2)"]
        let moduleScopeTrees = makeModuleScopeTrees (singleFileProject source)
                
        match moduleScopeTrees with
            | [Declaration((("Test.TestModule1",_),["Test.TestModule1.TopLevelValue1",_;"Test.TestModule1.TopLevelValue2",_;"Test.TestModule1.TopLevelFunction3",_]),
                [Declaration((("Test.TestModule2",_),["Test.TestModule2.f",_]),[]);
                 Usage("Test.TestModule1.TopLevelValue2",r);
                 Usage("Test.TestModule1.TopLevelFunction3",_)])] when r = mkRange "test.fs" (mkPos 8 10) (mkPos 8 36) -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source moduleScopeTrees)
            
    [<Test>]
    member this.``Can get a list of modules from a piece of source code``() =
        let source =
            String.concat "\n" ["namespace Test";
                                "module M1 =";
                                "  let D1 = 1";
                                "  let D2 = 2";
                                "module M2 =";
                                "  let D2 = 2";
                                "  module M3 =";
                                "    let D4 = 4"]
        let modules = Modules.GetModules (new Project(source, "test.fs"))
        let expected =
            [{
                fullName = "Test", ["M1"];
                declarations = ["D1"; "D2"];
                nestedModules = [];
                filename = "test.fs"
            };
            {
                fullName = "Test", ["M2"];
                declarations = ["D2"];
                nestedModules = 
                    [{
                        fullName = "Test", ["M2";"M3"];
                        declarations = ["D4"];
                        nestedModules = [];
                        filename = "test.fs"
                    }];
                filename = "test.fs"
            }]
        
        Assert.AreEqual(expected, modules, sprintf "%A" modules)
        
    [<Test>]
    member this.``Can get a list of modules from an entire project``() =
        let files = [files.[0]; files.[1]]
        let project = new Project(files.[0], List.zip files [None; None] |> List.toArray)
        let modules = Modules.GetModules project
        let expected =
            [{
                fullName = "Test", ["TestModule1"];
                declarations = ["TopLevelFunction1"; "TopLevelFunction2"];
                nestedModules = [];
                filename = files.[0]
            };
            {
                fullName = "Test", ["TestModule2"];
                declarations = ["TopLevelFunction1"];
                nestedModules = [];
                filename = files.[0]
            };
            {
                fullName = "Test", ["TestModule3"];
                declarations = ["TopLevelFunction3"];
                nestedModules = [];
                filename = files.[1]
            }]
        
        Assert.AreEqual(expected, modules, sprintf "%A" modules)