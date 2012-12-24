namespace FSharpRefactor.Tests.EngineTests

open System
open NUnit.Framework
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.CodeAnalysis


[<TestFixture>]
type AstModule() =
    [<Test>]
    member this.``Can instantiate a AstNode from the result of parsing source code``() =
        let source = "let a = 1"
        let rootNode = Ast.Parse source
        let expectNamespace node =
            match node with
                | Some(Ast.AstNode.File(_)) -> true
                | _ -> false
        Assert.IsTrue(expectNamespace rootNode, "Should get a namespace or module from MakeAstNode")

        let binding (node : Ast.AstNode option) =
            Ast.GetChildren(List.head(Ast.GetChildren(node.Value).Value))
        let expectModule (node : Ast.AstNode option) =
            match binding node with
                | Some(Ast.AstNode.ModuleDeclaration(_)::_) -> true
                | _ -> false
        Assert.IsTrue(expectModule rootNode)

[<TestFixture>]
type CodeTransformsModule() =
    [<Test>]
    member this.``Can change the text corresponding to an ast node``() =
        let source = "let a = 1\n\n"
        let tree = (Ast.Parse source).Value
        let a = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.Head).Value.Head).Value.Head
        let expected = "let b = 1\n\n"

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [((Ast.GetRange a).Value,"b")])

    [<Test>]
    member this.``Can change the text for two ast nodes``() =
        let source = "\nlet a = 1\nlet b = 2"
        let tree = (Ast.Parse source).Value
        let a = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.Head).Value.Head).Value.Head
        let b = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.[1]).Value.Head).Value.Head
        let expected = "\nlet b = 1\nlet a2 = 2"

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [((Ast.GetRange a).Value,"b");((Ast.GetRange b).Value,"a2")])

    [<Test>]
    member this.``Can get text at a given range``() =
        let source = "let a = 1+(2+3)+4"
        let expected = "(2+3)"
        let range = mkRange "/home/lewis/test.fs" (mkPos 1 10) (mkPos 1 15)
        
        Assert.AreEqual(expected, CodeTransforms.TextOfRange source range)

        let source = "let a = 1+(2\n+3)+4"
        let expected = "(2\n+3)"
        let range = mkRange "/home/lewis/test.fs" (mkPos 1 10) (mkPos 1 16)

        Assert.AreEqual(expected, CodeTransforms.TextOfRange source range)



[<TestFixture>]
type ScopeAnalysisModule() =
    let getTrees source = ScopeAnalysis.makeScopeTrees (Ast.Parse source).Value
    
    [<Test>]
    member this.``Can get all the free identifiers names in a ScopeTree``() =
        let source1 = "let a = b in f(c + a + b)"
        let source2 = "let a = b in let b = 1"
        let source3 = "let a b = b in f(c + (a 1) + d)"
        let expected1 = Set(["b"; "c"; "f"])
        let expected2 = Set(["b"])
        let expected3 = Set(["c"; "d"; "f"])
        let assertFun (source, expected) =
            let tree = getTrees source
            let actual = ScopeAnalysis.GetFreeIdentifiers tree (Set ["op_Addition"])
            Assert.AreEqual(expected, actual, sprintf "%A" actual)

        assertFun (source1,expected1)
        assertFun (source2,expected2)
        assertFun (source3,expected3)        

    [<Test>]
    member this.``Can get all the declared names in a ScopeTree``() =
        let source = "let a = let b = c + 1 in let c = 3 in a"
        let expected = Set ["a";"b";"c"]
        let tree = getTrees source

        Assert.AreEqual(expected, ScopeAnalysis.GetDeclarations tree)

[<TestFixture>]
type ScopeTreeModule() =
    static member getScopeTrees source =
        ScopeAnalysis.makeScopeTrees (Ast.Parse source).Value
    
    [<Test>]
    member this.``Creates a scope tree for a simple let statement``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a = 1 in a"
        match scopeTrees with
            | [ScopeAnalysis.Declaration(["a",_],[ScopeAnalysis.Usage("a",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a = 1 in a' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Creates a scope tree for a more elaborate sequence of let statements``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let b = 1\n  let c = 2 + b + b\n  let d = 1\n  b+c\nlet b = 3+a"
        match scopeTrees with
            | [ScopeAnalysis.Declaration(["a",_],
                                     [ScopeAnalysis.Declaration(["b",_],[]);
                                      ScopeAnalysis.Usage("op_Addition",_);
                                      ScopeAnalysis.Usage("a",_)]);
               ScopeAnalysis.Declaration(["b",_],
                                     [ScopeAnalysis.Declaration(["c",_],
                                                            [ScopeAnalysis.Declaration(["d",_],
                                                                                   [ScopeAnalysis.Usage("op_Addition",_);
                                                                                    ScopeAnalysis.Usage("b",_);
                                                                                    ScopeAnalysis.Usage("c",_)])]);
                                      ScopeAnalysis.Usage("op_Addition",_);
                                      ScopeAnalysis.Usage("op_Addition",_);
                                      ScopeAnalysis.Usage("b",_);
                                      ScopeAnalysis.Usage("b",_)])] -> ()
            
            | _ -> Assert.Fail("The scope tree for elaborate let expression was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a match clause``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "match a with (a,b) -> a"
        match scopeTrees with
            | [ScopeAnalysis.Usage("a",_);
               ScopeAnalysis.Declaration([("a",_);("b",_)],
                                     [ScopeAnalysis.Usage("a",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'match a with (a,b) -> a' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a function declaration``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let f a b c = a in f 1"
        match scopeTrees with
            | [ScopeAnalysis.Declaration([("f",_)],[ScopeAnalysis.Usage("f",_)]);
               ScopeAnalysis.Declaration([("a",_);("b",_);("c",_)],[ScopeAnalysis.Usage("a",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let f a b c = a in f 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for two mutually recursive identifiers``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f = g\nand g = f"
        match scopeTrees with
            | [ScopeAnalysis.Declaration([("f",_);("g",_)],
                                         [ScopeAnalysis.Usage("g",_);
                                          ScopeAnalysis.Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for\n 'let rec f = g\nand g = f'\n was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for two mutually recursive identifiers in a LetOrUse``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let rec f = g\n  and g = f\n  f"
        match scopeTrees with
            | [ScopeAnalysis.Declaration([("a",_)],[]);
               ScopeAnalysis.Declaration([("f",_);("g",_)],
                                         [ScopeAnalysis.Usage("g",_);
                                          ScopeAnalysis.Usage("f",_);
                                          ScopeAnalysis.Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for\n 'let a =\n  let rec f = g\n  and g = f\n  f'\n was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a single recursive identifier``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f = f"
        match scopeTrees with
            | [ScopeAnalysis.Declaration([("f",_)],[ScopeAnalysis.Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let rec f = f' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can create a scope tree for a single recursive LetOrUse``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let a =\n  let rec f = f\n  f"
        match scopeTrees with
            | [ScopeAnalysis.Declaration([("a",_)],[]);
               ScopeAnalysis.Declaration([("f",_)],[ScopeAnalysis.Usage("f",_);
                                                    ScopeAnalysis.Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let a =\n  let rec f = f\n  f' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

                                                    
    [<Test>]
    member this.``Can create a scope tree for single recursive function``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "let rec f x = f x in f 1"
        match scopeTrees with
            | [ScopeAnalysis.Declaration([("f",_)],
                                         [ScopeAnalysis.Declaration([("x",_)],
                                                                    [ScopeAnalysis.Usage("f",_);
                                                                     ScopeAnalysis.Usage("x",_)]);
                                          ScopeAnalysis.Usage("f",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'let rec f x = f x in f 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))
