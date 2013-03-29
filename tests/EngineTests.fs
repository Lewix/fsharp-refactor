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
    member this.``Can indent a string``() =
        let indent body = CodeTransforms.Indent body "    "
        Assert.AreEqual("    hello", indent "hello")
        Assert.AreEqual("    \n    ", indent "\n")
        Assert.AreEqual("    line one\n    line two\n    ", indent "line one\nline two\n")
        Assert.AreEqual("aaaaahello", CodeTransforms.Indent "hello" "aaaaa")

    [<Test>]
    member this.``Can remove leading characters from lines if they all the same``() =
        Assert.AreEqual(" Hello\nWorld", CodeTransforms.RemoveLeading ' ' "    Hello\n   World")
        Assert.AreEqual("Hello\nWorld", CodeTransforms.RemoveLeading ' ' "Hello\nWorld")

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
        let range = mkRange "test.fs" (mkPos 1 10) (mkPos 1 15)
        
        Assert.AreEqual(expected, CodeTransforms.TextOfRange source range)

        let source = "let a = 1+(2\n+3)+4"
        let expected = "(2\n+3)"
        let range = mkRange "test.fs" (mkPos 1 10) (mkPos 2 3)

        Assert.AreEqual(expected, CodeTransforms.TextOfRange source range)

    [<Test>]
    member this.``Can change text for two ranges with same start point, if one is of length 0``() =
        let source = "1"
        let expected = "5+2"
        let range1= mkRange "test.fs" (mkPos 1 0) (mkPos 1 1)
        let range2 = mkRange "test.fs" (mkPos 1 0) (mkPos 1 0)

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [(range2,"5+");(range1,"2")])

    [<Test>]
    member this.``Can compute output declarationIdentifier from input one``() =
        let source = "let a = 1 in let b = 2 in a"
        let expected = "longIdentifier", mkRange "test.fs" (mkPos 1 17) (mkPos 1 31)
        let declarationRange = mkRange "test.fs" (mkPos 1 17) (mkPos 1 18)
        Assert.AreEqual(expected, CodeTransforms.updateIdentifier ("b",declarationRange) "longIdentifier")

    [<Test>]
    member this.``Can create an identifier starting at a given position``() =
        let expected = "longIdentifier", mkRange "test.fs" (mkPos 1 17) (mkPos 1 31)
        Assert.AreEqual(expected, CodeTransforms.createIdentifier (1, 17) "longIdentifier" "test.fs")

[<TestFixture>]
type ScopeAnalysisModule() =
    let getTrees source = ScopeAnalysis.makeScopeTrees (Ast.Parse source).Value

    [<Test>]
    member this.``Can find an unused name``() =
        let source = "let f a b c = 1\nlet g = 3"
        let tree = (Ast.Parse source).Value
        
        Assert.IsFalse(Set.contains (ScopeAnalysis.FindUnusedName tree) (Set ["f";"a";"b";"c";"g"]))

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

    [<Test>]
    member this.``Can find the declaration identifier from a given identifier``() =
        let source = "let f a b c = a"
        let usageIdentifier = ("a", mkRange "test.fs" (mkPos 1 14) (mkPos 1 15))
        let expected = Some("a", mkRange "test.fs" (mkPos 1 6) (mkPos 1 7))
        let trees = getTrees source

        Assert.AreEqual(expected, ScopeAnalysis.TryFindIdentifierDeclaration trees usageIdentifier)

[<TestFixture>]
type ScopeTreeModule() =
    static member getScopeTrees source =
        ScopeAnalysis.makeScopeTrees (Ast.Parse source).Value

    [<Test>]
    member this.``Can create a scope tree for a class with implicit constructor``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass(x, y) = member self.x = x"
        match scopeTrees with
            | [ScopeAnalysis.Declaration(["x",_;"y",_],
                [ScopeAnalysis.Declaration(["self",_;"x",_],[]);
                 ScopeAnalysis.Usage("x",_)])] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass(x, y) = member self.x = x' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))
                
    [<Test>]
    member this.``Can recognise declared identifiers in LongIdentWithDots``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "type TestClass = member self.x = 1"
        match scopeTrees with
            | [ScopeAnalysis.Declaration(["self",_;"x",_],_)] -> ()
            | _ -> Assert.Fail("The scope tree for 'type TestClass = member self.x = 1' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

    [<Test>]
    member this.``Can recognise used identifiers in LongIdentWithDots``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "ident1.ident2"
        match scopeTrees with
            | [ScopeAnalysis.Usage("ident1",_); ScopeAnalysis.Usage("ident2",_)] -> ()
            | _ -> Assert.Fail("The scope tree for 'ident1.ident2' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))
    
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
    member this.``Can create scope trees for a match clause with a LongIdent``() =
        let scopeTrees1 = ScopeTreeModule.getScopeTrees "match a with Tag(id) -> id"
        let scopeTrees2 = ScopeTreeModule.getScopeTrees "match a with Tag(id)::_ -> id"
        let doMatch scopeTrees =
            match scopeTrees with
                | [ScopeAnalysis.Usage("a",_);
                   ScopeAnalysis.Declaration(["id",_],
                                             [ScopeAnalysis.Usage("id",_)])] -> ()
                | _ -> Assert.Fail("The scope tree for 'match a with Tag(id) -> id' was incorrect:\n" +
                                   (sprintf "%A" scopeTrees))

        doMatch scopeTrees1
        doMatch scopeTrees2

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

    [<Test>]
    member this.``Can create a scope tree for a lambda expression``() =
        let scopeTrees = ScopeTreeModule.getScopeTrees "fun a (b,c) -> a b c"
        match scopeTrees with
            | [ScopeAnalysis.Declaration([("a",_)],
                                         [ScopeAnalysis.Declaration([("b",_);("c",_)],
                                                                    [ScopeAnalysis.Usage("a",_);
                                                                     ScopeAnalysis.Usage("b",_);
                                                                     ScopeAnalysis.Usage("c",_)])])] ->
                ()
            | _ -> Assert.Fail("The scope tree for 'fun a (b,c) -> a b c' was incorrect:\n" +
                               (sprintf "%A" scopeTrees))

[<TestFixture>]
type RangeAnalysisModule() =
    [<Test>]
    member this.``Can find identifier a position``() =
        let filename = "test.fs"
        let source = "let functio a b c = a+b+c in functio 1 2 3"
        let aDeclarationRange = mkRange "test.fs" (mkPos 1 12) (mkPos 1 13)
        let fDeclarationRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 11)

        Assert.AreEqual(Some("a",aDeclarationRange), RangeAnalysis.FindIdentifier source aDeclarationRange.Start)
        Assert.AreEqual(Some("functio",fDeclarationRange), RangeAnalysis.FindIdentifier source fDeclarationRange.Start)

    [<Test>]
    member this.``Can find identifier position when identifiers are just next to each other``() =
        let source = "a+b"
        let range = mkRange "test.fs" (mkPos 1 2) (mkPos 1 3)

        Assert.AreEqual(Some("b", range), RangeAnalysis.FindIdentifier source range.Start)
    
    [<Test>]
    member this.``Can find the AstNode.Expression corresponding to a range``() =
        let source = "let a = 1+(2+3)+4"
        let tree = (Ast.Parse source).Value
        let expressionRange = mkRange "test.fs" (mkPos 1 10) (mkPos 1 15)
        let expression = RangeAnalysis.TryFindExpressionAtRange expressionRange tree

        match expression with
            | Some(Ast.AstNode.Expression(SynExpr.Paren(SynExpr.App(_,_,SynExpr.App(_,_,_,SynExpr.Const(_,_),_),SynExpr.Const(_,_),_),_,_,_))) -> ()
            | _ -> Assert.Fail("The AstNode was not the one for (2+3): " + (sprintf "%A" expression))

    [<Test>]
    member this.``Can find the binding from the binding's range``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 15)
        let binding = RangeAnalysis.FindBindingAtRange bindingRange tree

        match binding with
            | SynBinding.Binding(_,_,_,_,_,_,_,SynPat.LongIdent(_,_,_,_,_,_),_,_,_,_) -> ()
            | _ -> Assert.Fail("The AstNode was not the one for the binding 'let f a b = a+b': " + (sprintf "%A" binding))
