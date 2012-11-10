namespace FSharpRefactor.Engine.Ast

open System
open Microsoft.FSharp.Compiler.Ast

module Ast =
    type AstNode =
        | Expr of SynExpr
        | ModuleOrNamespace of SynModuleOrNamespace
        | Module of SynModuleDecl
        | Binding of SynBinding
        | File of ParsedImplFileInput

    let MakeAstNode (tree : ParsedInput option) =
        match tree with
            | Some(ParsedInput.ImplFile(f)) -> Some(AstNode.File(f))
            | _ -> raise (new NotImplementedException("Use an impl file instead of a sig file"))


    // Active patterns to make dealing with the syntax tree more convenient
    let (|ModuleOrNamespaceChildren|_|) (expression : SynModuleOrNamespace) =
        match expression with
            | SynModuleOrNamespace (_,_,e1,_,_,_,_) -> Some(e1)

        
    let (|SynExprChildren|_|) (expression : SynExpr) =
        match expression with
            | SynExpr.Paren(e1,_,_,_) -> Some([e1])
            | SynExpr.Quote(e1,_,e2,_,_) -> Some([e1;e2])
            | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for SynExpr children"))

    let (|Children|_|) (node : AstNode) =
        match node with
            | File(ParsedImplFileInput(_,_,_,_,_,ns,_)) -> Some(List.map AstNode.ModuleOrNamespace ns)
            | ModuleOrNamespace(ns) ->
                match ns with
                    | ModuleOrNamespaceChildren(modules) -> Some(List.map AstNode.Module modules)
                    | _ -> None
            | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for Children"))

    let (|Range|_|) (node : AstNode) = raise (new NotImplementedException())

    // Utility functions to avoid having to match patterns to get children or range
    let GetChildren (node : AstNode) =
        match node with
            | Children(c) -> Some(c)
            | _ -> None

    let GetRange (node : AstNode) =
        match node with
            | Range(r) -> r
            | _ -> None
