namespace FSharpRefactor.Engine.Ast

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

module Ast =
//TODO: Make my own range so I don't need to use the compiler one
    
    type AstNode =
        | Expression of SynExpr
        | Pattern of SynPat
        | ModuleOrNamespace of SynModuleOrNamespace
        | Module of SynModuleDecl
        | Binding of SynBinding
        | File of ParsedImplFileInput

    let MakeAstNode (tree : ParsedInput option) =
        match tree with
            | Some(ParsedInput.ImplFile(f)) -> Some(AstNode.File(f))
            | _ -> raise (new NotImplementedException("Use an impl file instead of a sig file"))

    let getParseTree source = 
        let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))
        let filename = "/home/lewis/test.fs"
        let options source = checker.GetCheckOptionsFromScriptRoot(filename, source, DateTime.Now, [| |])
        checker.UntypedParse(filename, source, options source).ParseTree
        
    let Parse source = MakeAstNode (getParseTree source)

    // Active patterns to make dealing with the syntax tree more convenient
    let (|ModuleOrNamespaceChildren|_|) (expression : SynModuleOrNamespace) =
        match expression with
            | SynModuleOrNamespace (_,_,e1,_,_,_,_) -> Some(e1)

        
    let (|SynExprChildren|_|) (expression : SynExpr) =
        match expression with
            | SynExpr.Paren(e1,_,_,_) -> Some([e1])
            | SynExpr.Quote(e1,_,e2,_,_) -> Some([e1;e2])
            | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for SynExpr children:" + (string expression)))

    let (|Children|_|) (node : AstNode) =
        match node with
            | File(ParsedImplFileInput(_,_,_,_,_,ns,_)) -> Some(List.map AstNode.ModuleOrNamespace ns)
            | ModuleOrNamespace(ns) ->
                match ns with
                    | ModuleOrNamespaceChildren(modules) -> Some(List.map AstNode.Module modules)
                    | _ -> None
            | Module(m) ->
                match m with
                    | SynModuleDecl.Let(_,bs,_) -> Some(List.map AstNode.Binding bs)
                    | _ -> raise (new NotImplementedException("Add a new entry to pattern for Module: " + (string m)))
            | Binding(b) ->
                match b with
                    | SynBinding.Binding(_,_,_,_,_,_,_,p,_,e,_,_) -> Some([AstNode.Pattern p; AstNode.Expression e])

            | Expression(e) ->
                match e with
                    | SynExpr.LetOrUse(_,_,bs,e,_) ->  Some(List.append (List.map AstNode.Binding bs) [AstNode.Expression e])
                    | _ -> raise (new NotImplementedException("Add a new entry to pattern for Expression: " + (string e)))
                    
            | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for Children:" + (string node)))

    let (|Range|_|) (node : AstNode) =
        match node with
            | File _ -> None
            | Pattern p -> Some(p.Range)
            | ModuleOrNamespace ns -> Some(ns.Range)
            | Module m -> Some(m.Range)
            | Binding b -> Some(b.RangeOfBindingAndRhs)
            | Expression e -> Some(e.Range)
            | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for Range:" + (string node)))

    // Utility functions to avoid having to match patterns to get children or range
    let GetChildren (node : AstNode) =
        match node with
            | Children(c) -> Some(c)
            | _ -> None

    let GetRange (node : AstNode) =
        match node with
            | Range(r) -> Some(r)
            | _ -> None

            
