namespace FSharpRefactorTests

open System
open NUnit.Framework
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.ScopeAnalysis
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.Scoping
open FSharpRefactor.Engine

[<TestFixture>]
type IdentifierScopeModule() =
    [<Test>]
    member this.``Can check whether an identifier is free in another's scope``() =
        let source = "let a b = b in f ((a 1) + (fun d -> d + c))"
        let identifierScope = GetIdentifierScope (new Project(source, "test.fs")) ("a", mkRange "test.fs" (mkPos 1 4) (mkPos 1 5))
        
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
        let identifierScope = GetIdentifierScope (new Project(source, "test.fs")) usageIdentifier

        Assert.AreEqual(expected, identifierScope.IdentifierDeclaration)

[<TestFixture>]
type ScopeAnalysisModule() =
    let getTrees source = makeScopeTrees (Ast.Parse source "test.fs").Value

    [<Test>]
    member this.``Can find an unused name``() =
        let source = "let f a b c = 1\nlet g = 3"
        let tree = (Ast.Parse source "test.fs").Value
        
        Assert.IsFalse(Set.contains (FindUnusedName tree) (Set ["f";"a";"b";"c";"g"]))

    [<Test>]
    member this.``Can get all the free identifiers names in a ScopeTree``() =
        let source1 = "let a = b in f(c + a + b)"
        let source2 = "let a = b in let b = 1"
        let source3 = "let a b = b in f(c + (a 1) + d)"
        let expected1 = Set(["b"; "c"; "f"; "op_Addition"])
        let expected2 = Set(["b"])
        let expected3 = Set(["c"; "d"; "f"; "op_Addition"])
        let assertFun (source, expected) =
            let scope = new ExpressionScope((Ast.Parse source "test.fs").Value, new Project("test.fs", source))
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

[<TestFixture>]
type ScopeTreeModule() =
    static member getScopeTrees source =
        makeScopeTrees (Ast.Parse source "test.fs").Value
        
    [<Test>]
    member this.``Can create a scope tree for a match clause with a when expression``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "match a with\n  | b when c -> d"
        match scopeTrees with
            | [Declaration(["b",_],[Usage("c",_); Usage("d",_)]);Usage("a",_)]
                -> ()
            | _ -> Assert.Fail("The scope tree for 'match a with\n  | b when c -> d' was incorrect:\n" + (sprintf "%A" scopeTrees))
    
    [<Test>]
    member this.``Can create a scope tree for a nested module``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "module nestedModule =\n  let x = 1\n  let y = x"
        match scopeTrees with
            | [TopLevelDeclaration(["x",_],[TopLevelDeclaration(["y",_],[]);Usage("x",_)])]
                -> ()
            | _ -> Assert.Fail("The scope tree for 'module nestedModule =\n  let x = 1\n let y = x' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a class with an implicit inherit``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass(x) = \n  inherit BaseClass(x)\n  let a = x"
        match scopeTrees with
            | [Declaration(["x",_],[Usage("x",_);
                                                  Declaration(["a",_],[]);
                                                  Usage("x",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass(x) = inherit BaseClass(x)' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a class with a self identifier in the implicit constructor``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass(x, y) as this = let a = this"
        match scopeTrees with
            | [Declaration(["this",_;"x",_;"y",_],
                [Declaration(["a",_],[]);
                 Usage("this",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass(x, y) as this = let a = this' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a class with implicit constructor``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass(x, y) = member self.x = x"
        match scopeTrees with
            | [Declaration(["x",_;"y",_],
                [Declaration(["self",_],[Usage("x",_)])])] -> ()
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
    member this.``Can recognise used identifiers in LongIdentWithDots``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "ident1.ident2"
        match scopeTrees with
            | [Usage("ident1",_)] -> ()
            | _ -> Assert.Fail("The scope tree for 'ident1.ident2' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))
    
    [<Test>]
    member this.``Creates a scope tree for a simple let statement``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a = 1 in a"
        match scopeTrees with
            | [Declaration(["a",_],[Usage("a",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a = 1 in a' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Creates a scope tree for a more elaborate sequence of let statements``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let b = 1\n  let c = 2 + b + b\n  let d = 1\n  b+c\nlet b = 3+a"
        match scopeTrees with
            | [TopLevelDeclaration(["a",_],
                                     [TopLevelDeclaration(["b",_],[]);
                                      Usage("op_Addition",_);
                                      Usage("a",_)]);
               Declaration(["b",_],
                                     [Declaration(["c",_],
                                                            [Declaration(["d",_],
                                                                                   [Usage("op_Addition",_);
                                                                                    Usage("b",_);
                                                                                    Usage("c",_)])]);
                                      Usage("op_Addition",_);
                                      Usage("op_Addition",_);
                                      Usage("b",_);
                                      Usage("b",_)])] -> ()
            
            | _ -> Assert.Fail("The scope tree for elaborate let expression was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a match clause``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "match a with (a,b) -> a"
        match scopeTrees with
            | [Declaration([("a",_);("b",_)],
                                     [Usage("a",_)]);
               Usage("a",_)] -> ()
            | _ -> Assert.Fail("The scope tree for 'match a with (a,b) -> a' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create scope trees for a match clause with a LongIdent``() =
        let scopeTrees1 = ScopeTreeModule.getScopeTrees "match a with Tag(id) -> id"
        let scopeTrees2 = ScopeTreeModule.getScopeTrees "match a with Tag(id)::_ -> id"
        let doMatch scopeTrees =
            match scopeTrees with
                | [Declaration(["id",_],
                                             [Usage("id",_)]);
                   Usage("a",_)] -> ()
                | _ -> Assert.Fail("The scope tree for 'match a with Tag(id) -> id' was incorrect:\n" +
                                   (sprintf "%A" scopeTrees))

        doMatch scopeTrees1
        doMatch scopeTrees2

    [<Test>]
    member this.``Can create a scope tree for a function declaration``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let f a b c = a in f 1"
        match scopeTrees with
            | [Declaration([("f",_)],[Usage("f",_)]);
               Declaration([("a",_);("b",_);("c",_)],[Usage("a",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let f a b c = a in f 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for two mutually recursive identifiers``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f = g\nand g = f"
        match scopeTrees with
            | [TopLevelDeclaration([("f",_);("g",_)],
                                         [Usage("g",_);
                                          Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for\n 'let rec f = g\nand g = f'\n was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for two mutually recursive identifiers in a LetOrUse``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let rec f = g\n  and g = f\n  f"
        match scopeTrees with
            | [TopLevelDeclaration([("a",_)],[]);
               Declaration([("f",_);("g",_)],
                                         [Usage("g",_);
                                          Usage("f",_);
                                          Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for\n 'let a =\n  let rec f = g\n  and g = f\n  f'\n was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a single recursive identifier``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f = f"
        match scopeTrees with
            | [TopLevelDeclaration([("f",_)],[Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let rec f = f' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a single recursive LetOrUse``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let rec f = f\n  f"
        match scopeTrees with
            | [TopLevelDeclaration([("a",_)],[]);
               Declaration([("f",_)],[Usage("f",_);
                                                    Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a =\n  let rec f = f\n  f' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

                                                    
    [<Test>]
    member this.``Can create a scope tree for single recursive function``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f x = f x in f 1"
        match scopeTrees with
            | [Declaration([("f",_)],
                                         [Declaration([("x",_)],
                                                                    [Usage("f",_);
                                                                     Usage("x",_)]);
                                          Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let rec f x = f x in f 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a lambda expression``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "fun a (b,c) -> a b c"
        match scopeTrees with
            | [Declaration([("a",_)],
                                         [Declaration([("b",_);("c",_)],
                                                                    [Usage("a",_);
                                                                     Usage("b",_);
                                                                     Usage("c",_)])])] ->
                ()
            | _ -> Assert.Fail("The scope tree for 'fun a (b,c) -> a b c' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))
