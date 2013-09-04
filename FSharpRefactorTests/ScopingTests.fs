namespace FSharpRefactorTests

open System
open System.IO
open NUnit.Framework
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.ScopeAnalysis
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.Scoping
open FSharpRefactor.Engine.Projects
open FSharpRefactor.Engine.ReferenceFinder
open FSharpRefactor.Engine

[<TestFixture>]
type IdentifierScopeModule() =
    let mkRange filename startPos endPos = mkRange (Path.GetFullPath filename) startPos endPos

    let files = ["test.fs"]
    [<SetUp>]
    member this.CreateFiles () =
        List.map (fun (f:string) -> new StreamWriter(f)) files
        |> List.map (fun (s:StreamWriter) -> s.Close())
        |> ignore
    [<TearDown>]
    member this.DeleteFiles () =
        List.map File.Delete files
        |> ignore

    [<Test>]
    member this.``Can check whether an identifier is free in another's scope``() =
        let source = "let a b = b in f ((a 1) + (fun d -> d + c))"
        let identifierScope = GetIdentifierScope (new Project(source, "test.fs")) (("a", mkRange "test.fs" (mkPos 1 4) (mkPos 1 5)), [])
        
        Assert.IsTrue(identifierScope.IsFree "f")
        Assert.IsTrue(identifierScope.IsFree "c")
        Assert.IsFalse(identifierScope.IsFree "d")
        Assert.IsFalse(identifierScope.IsFree "a")
        Assert.IsFalse(identifierScope.IsFree "unusedVariableName")
    
    [<Test>]
    member this.``Can find the declaration of an identifier from IdentifierScope``() =
        let source = "let f a b c = a"
        let usageIdentifier = ("a", mkRange "test.fs" (mkPos 1 14) (mkPos 1 15))
        let expected = "a", mkRange "test.fs" (mkPos 1 6) (mkPos 1 7)
        let identifierScope = GetIdentifierScope (new Project(source, "test.fs")) (usageIdentifier, [])

        Assert.AreEqual(expected, identifierScope.IdentifierDeclaration)
        
    [<Test>]
    member this.``Can get the identifier scope from a long identifier usage``() =
        let source =
            String.concat "\n" ["namespace Test"
                                "module TestModule = "
                                "  let f = 1"
                                "module TestModule2 ="
                                "  let g = TestModule.f"]
        let usageIdentifier = ("TestModule", mkRange "test.fs" (mkPos 5 10) (mkPos 5 20)), ["f", mkRange "test.fs" (mkPos 5 21) (mkPos 5 22)]
        let expected = "f", mkRange "test.fs" (mkPos 3 6) (mkPos 3 7)
        let identifierScope = GetIdentifierScope (new Project(source, "test.fs")) usageIdentifier
        
        Assert.AreEqual(expected, identifierScope.IdentifierDeclaration)

[<TestFixture>]
type ScopeAnalysisModule() =
    let parse (source:string) (filename:string) =
        GetParseTree (new Project(source, filename)) filename

    let getTrees source = makeProjectScopeTrees (new Project(source, "test.fs")) (parse source "test.fs")
    
    let mkRange filename startPos endPos = mkRange (Path.GetFullPath filename) startPos endPos

    [<Test>]
    member this.``Can find an unused name``() =
        let source = "let f a b c = 1\nlet g = 3"
        let tree = parse source "test.fs"
        
        Assert.IsFalse(Set.contains (FindUnusedName (new Project(source, "test.fs")) tree) (Set ["f";"a";"b";"c";"g"]))

    [<Test>]
    member this.``Can get all the free identifiers names in a ScopeTree``() =
        let source1 = "let a = b in f(c + a + b)"
        let source2 = "let a = b in let b = 1"
        let source3 = "let a b = b in f(c + (a 1) + d)"
        let expected1 = Set(["b"; "c"; "f"; "op_Addition"])
        let expected2 = Set(["b"])
        let expected3 = Set(["c"; "d"; "f"; "op_Addition"])
        let assertFun (source, expected) =
            let scope = new ExpressionScope((parse source "test.fs"), new Project(source, "test.fs"))
            let actual =
                scope.FindFreeIdentifiers ()
                |> List.map fst
                |> Set.ofList
            Assert.AreEqual(expected, actual, sprintf "%A" actual)

        assertFun (source1,expected1)
        assertFun (source2,expected2)
        assertFun (source3,expected3)

    [<Test>]
    member this.``Can get all the declared names in a ScopeTree``() =
        let source = "let a = let b = c + 1 in let c = 3 in a"
        let expected = Set ["a";"b";"c"]
        let tree = getTrees source

        Assert.AreEqual(expected, getDeclarations tree)

    [<Test>]
    member this.``Can find the declaration identifier from a given identifier``() =
        let source = "let f a b c = a"
        let usageIdentifier = ("a", mkRange "test.fs" (mkPos 1 14) (mkPos 1 15))
        let expected = Some("a", mkRange "test.fs" (mkPos 1 6) (mkPos 1 7))
        let trees = getTrees source

        Assert.AreEqual(expected, tryFindIdentifierDeclaration trees usageIdentifier)
        
    [<Test>]
    member this.``Can determine whether a declaration is accessible outside the file it's declared in``() =
        let source = "module Test\nlet f a = let x = 2 in x\nlet x = 3"
        let project = new Project(source, "test.fs")
        let filename = Path.GetFullPath "test.fs"
        
        Assert.IsTrue(declarationEscapesFile project ("f", mkRange filename (mkPos 2 4) (mkPos 2 5)))
        Assert.IsFalse(declarationEscapesFile project ("a", mkRange filename (mkPos 2 6) (mkPos 2 7)))
        Assert.IsFalse(declarationEscapesFile project ("x", mkRange filename (mkPos 2 14) (mkPos 2 15)))
        Assert.IsTrue(declarationEscapesFile project ("x", mkRange filename (mkPos 3 4) (mkPos 3 5)))

[<TestFixture>]
type ScopeTreeModule() =
    let incorrectScopeTrees source scopeTrees =
        sprintf "ModuleScopeTrees for '%s' were incorrect:\n %A" source scopeTrees

    static member getScopeTrees source =
        let parse (source:string) (filename:string) =
            GetParseTree (new Project(source, filename)) filename
        makeProjectScopeTrees (new Project(source, "test.fs")) (parse source "test.fs")
        
    [<Test>]
    member this.``Can create a scope tree for a match clause with a when expression``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "match a with\n  | b when c -> d"
        match scopeTrees with
            | [Usage(("a",_),[]); Declaration(["b",_],[Usage(("c",_),[]); Usage(("d",_),[])])]
                -> ()
            | _ -> Assert.Fail("The scope tree for 'match a with\n  | b when c -> d' was incorrect:\n" + (sprintf "%A" scopeTrees))
    
    [<Test>]
    member this.``Can create a scope tree for a nested module``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "module nestedModule =\n  let x = 1\n  let y = x"
        match scopeTrees with
            | [Declaration(["x",_],[Declaration(["y",_],[]);Usage(("x",_),[])])]
                -> ()
            | _ -> Assert.Fail("The scope tree for 'module nestedModule =\n  let x = 1\n let y = x' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a class with an implicit inherit``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass(x) = \n  inherit BaseClass(x)\n  let a = x"
        match scopeTrees with
            | [Declaration(["x",_],[Usage(("x",_),[]);
                                    Declaration(["a",_],[]);
                                    Usage(("x",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass(x) = inherit BaseClass(x)' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a class with a self identifier in the implicit constructor``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass(x, y) as this = let a = this"
        match scopeTrees with
            | [Declaration(["this",_;"x",_;"y",_],
                [Declaration(["a",_],[]);
                 Usage(("this",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass(x, y) as this = let a = this' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a class with implicit constructor``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass(x, y) = member self.x = x"
        match scopeTrees with
            | [Declaration(["x",_;"y",_],
                [Declaration(["self",_],[Usage(("x",_),[])])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass(x, y) = member self.x = x' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can recognise declared identifiers in LongIdentWithDots``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass = member self.x = 1"
        match scopeTrees with
            | [Declaration(["self",_],_)] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass = member self.x = 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))
            
    [<Test>]
    member this.``Can create a scope tree for a type with multiple members``() =
        let source = "type TestClass(x, y) =\n  member self.x a = 1\n  member self.y b = 2"
        let scopeTrees  = ScopeTreeModule.getScopeTrees source
        match scopeTrees with
            | [Declaration(["x",_;"y",_],
                [Declaration(["self",_;"b",_],[]);
                 Declaration(["self",_;"a",_],[])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source scopeTrees)
            
    [<Test>]
    member this.``Can create a scope tree for a type with multiple members with no named arguments``() =
        let source = "type TestClass(x, y) =\n  member member self.x = x\n  static member self.y () = y"
        let scopeTrees = ScopeTreeModule.getScopeTrees source
        match scopeTrees with
            | [Declaration(["x",_;"y",_],
                [Declaration(["self",_],[Usage(("y",_),[])]);
                 Declaration(["self",_],[Usage(("x",_),[])])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source scopeTrees)

    [<Test>]
    member this.``Can create scope trees for multiple types in a namespace``() =
        let source = "namespace TN\n\ntype Type1(x, y) = member self.x = x\n\ntype Type2(z, t) = member self.z = z"
        let scopeTrees = ScopeTreeModule.getScopeTrees source
        match scopeTrees with
            | [Declaration(["x",_;"y",_],[Declaration(["self",_],[Usage(("x",_),[])])]);
               Declaration(["z",_;"t",_],[Declaration(["self",_],[Usage(("z",_),[])])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source scopeTrees)

    [<Test>]
    member this.``Can recognise used identifiers in LongIdentWithDots``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "ident1.ident2"
        match scopeTrees with
            | [Usage(("ident1",_),["ident2",_])] -> ()
            | _ -> Assert.Fail("The scope tree for 'ident1.ident2' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))
    
    [<Test>]
    member this.``Creates a scope tree for a simple let statement``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a = 1 in a"
        match scopeTrees with
            | [Declaration(["a",_],[Usage(("a",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a = 1 in a' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Creates a scope tree for a more elaborate sequence of let statements``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let b = 1\n  let c = 2 + b + b\n  let d = 1\n  b+c\nlet b = 3+a"
        match scopeTrees with
            | [Declaration(["a",_],
                                     [Declaration(["b",_],[]);
                                      Usage(("op_Addition",_),[]);
                                      Usage(("a",_),[])]);
               Declaration(["b",_],
                                     [Declaration(["c",_],
                                                            [Declaration(["d",_],
                                                                                   [Usage(("op_Addition",_),[]);
                                                                                    Usage(("b",_),[]);
                                                                                    Usage(("c",_),[])])]);
                                      Usage(("op_Addition",_),[]);
                                      Usage(("op_Addition",_),[]);
                                      Usage(("b",_),[]);
                                      Usage(("b",_),[])])] -> ()
            
            | _ -> Assert.Fail("The scope tree for elaborate let expression was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a match clause``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "match a with (a,b) -> a"
        match scopeTrees with
            | [Usage(("a",_),[]);
               Declaration([("a",_);("b",_)],
                           [Usage(("a",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'match a with (a,b) -> a' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create scope trees for a match clause with a LongIdent``() =
        let scopeTrees1 = ScopeTreeModule.getScopeTrees "match a with Tag(id) -> id"
        let scopeTrees2 = ScopeTreeModule.getScopeTrees "match a with Tag(id)::_ -> id"
        let doMatch scopeTrees =
            match scopeTrees with
                | [Usage(("a",_),[]);
                   Declaration(["id",_],
                               [Usage(("id",_),[])]);] -> ()
                | _ -> Assert.Fail("The scope tree for 'match a with Tag(id) -> id' was incorrect:\n" +
                                   (sprintf "%A" scopeTrees))

        doMatch scopeTrees1
        doMatch scopeTrees2

    [<Test>]
    member this.``Can create a scope tree for a function declaration``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let f a b c = a in f 1"
        match scopeTrees with
            | [Declaration([("f",_)],[Usage(("f",_),[])]);
               Declaration([("a",_);("b",_);("c",_)],[Usage(("a",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let f a b c = a in f 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for two mutually recursive identifiers``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f = g\nand g = f"
        match scopeTrees with
            | [Declaration([("f",_);("g",_)],
                                         [Usage(("g",_),[]);
                                          Usage(("f",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for\n 'let rec f = g\nand g = f'\n was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for two mutually recursive identifiers in a LetOrUse``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let rec f = g\n  and g = f\n  f"
        match scopeTrees with
            | [Declaration([("a",_)],[]);
               Declaration([("f",_);("g",_)],
                                         [Usage(("g",_),[]);
                                          Usage(("f",_),[]);
                                          Usage(("f",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for\n 'let a =\n  let rec f = g\n  and g = f\n  f'\n was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a single recursive identifier``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f = f"
        match scopeTrees with
            | [Declaration([("f",_)],[Usage(("f",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let rec f = f' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a single recursive LetOrUse``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let rec f = f\n  f"
        match scopeTrees with
            | [Declaration([("a",_)],[]);
               Declaration([("f",_)],[Usage(("f",_),[]);
                                                    Usage(("f",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a =\n  let rec f = f\n  f' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

                                                    
    [<Test>]
    member this.``Can create a scope tree for single recursive function``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f x = f x in f 1"
        match scopeTrees with
            | [Declaration([("f",_)],
                                         [Declaration([("x",_)],
                                                                    [Usage(("f",_),[]);
                                                                     Usage(("x",_),[])]);
                                          Usage(("f",_),[])])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let rec f x = f x in f 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a lambda expression``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "fun a (b,c) -> a b c"
        match scopeTrees with
            | [Declaration([("a",_)],
                                         [Declaration([("b",_);("c",_)],
                                                                    [Usage(("a",_),[]);
                                                                     Usage(("b",_),[]);
                                                                     Usage(("c",_),[])])])] ->
                ()
            | _ -> Assert.Fail("The scope tree for 'fun a (b,c) -> a b c' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a namespace with two nested modules``() =
        let source =
            String.concat "\n" ["namespace Test";
                                "module M1 =";
                                "  let f = 1";
                                "module M2 =";
                                "  let g = 2";
                                "  let h = 3"]
        let scopeTrees = ScopeTreeModule.getScopeTrees source
        match scopeTrees with
            | [Declaration(["f",_],[]);
               Declaration(["g",_],[Declaration([("h",_)],[])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source scopeTrees)
    
    [<Test>]
    member this.``Can create a scope tree for a module with an open statement``() =
        let files = ["test1.fs"; "test2.fs"]
        let fileWriters = List.map (fun (f:string) -> new StreamWriter(f)) files
        
        fprintfn fileWriters.[0] "namespace Test"
        fprintfn fileWriters.[0] "module TestModule1 ="
        fprintfn fileWriters.[0] "  let TopLevelFunction1 a = 1+a"
        fprintfn fileWriters.[0] "  let TopLevelFunction2 b = 2*(TopLevelFunction1 b)"
        fprintfn fileWriters.[0] "module TestModule2 ="
        fprintfn fileWriters.[0] "  let TopLevelFunction1 c = 3"
        
        fprintfn fileWriters.[1] "module TestModule3"
        fprintfn fileWriters.[1] "open Test.TestModule1"

        List.map (fun (f:StreamWriter) -> f.Flush()) fileWriters |> ignore
        List.map (fun (f:StreamWriter) -> f.Close()) fileWriters |> ignore

        let project = new Project(List.zip files [None; None] |> Seq.toArray)
        let scopeTrees = makeProjectScopeTrees project (GetParseTree project "test2.fs")
        
        match scopeTrees with
            | [Declaration(["TopLevelFunction1",_;"TopLevelFunction2",_],[])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees (project.GetContents "test2.fs") scopeTrees)
    
    [<Test>]
    member this.``Can create a scope tree for a module with various idents in open statements``() =
        let files = ["test1.fs"; "test2.fs"]
        let fileWriters = List.map (fun (f:string) -> new StreamWriter(f)) files
        
        fprintfn fileWriters.[0] "namespace Test"
        fprintfn fileWriters.[0] "module TestModule1 = let f = 1"

        fprintfn fileWriters.[1] "namespace Test2"
        fprintfn fileWriters.[1] "module TestModule2 = let g = 2"
        fprintfn fileWriters.[1] "module TestModule3 ="
        fprintfn fileWriters.[1] "  open Test.TestModule1"
        fprintfn fileWriters.[1] "  open TestModule2"

        List.map (fun (f:StreamWriter) -> f.Flush()) fileWriters |> ignore
        List.map (fun (f:StreamWriter) -> f.Close()) fileWriters |> ignore
        
        let project = new Project(List.zip files [None; None] |> Seq.toArray)
        let scopeTrees = makeProjectScopeTrees project (GetParseTree project "test2.fs")
        
        match scopeTrees with
            | [Declaration(["g",_],[]);
               Declaration(["f",_],
                [Declaration(["g",_],[])])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees (project.GetContents "test2.fs") scopeTrees)
            
    [<Test>]
    member this.``Can create a scope tree for a for-in loop``() =
        let source = "for a,b in pairs do\n  printfn \"%A\" a"
        let scopeTrees = ScopeTreeModule.getScopeTrees source
        match scopeTrees with
            | [Declaration(["a",_;"b",_],[Usage(("printfn",_),[]); Usage(("a",_),[])]); Usage(("pairs",_),[])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source scopeTrees)
            
    [<Test>]
    member this.``Can create s scope tree for a for-to loop``() =
        let source = "for i = a to b do\n  printfn \"%A\" i"
        let scopeTrees = ScopeTreeModule.getScopeTrees source
        match scopeTrees with
            | [Declaration(["i",_],[Usage(("printfn",_),[]); Usage(("i",_),[])]); Usage(("a",_),[]); Usage(("b",_),[])] -> ()
            | _ -> Assert.Fail(incorrectScopeTrees source scopeTrees)