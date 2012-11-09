namespace FSharpRefactor.Engine.Ast

open System
open Microsoft.FSharp.Compiler.Ast

module Ast =
    type AstNode =
        | Expr of SynExpr
        | ModuleOrNamespace of SynModuleOrNamespace
        | Binding of SynBinding


    let MakeAstNode (tree : ParsedInput option) = None

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
            | Expr(SynExprChildren(children)) -> None
            | ModuleOrNamespace(ModuleOrNamespaceChildren(children)) -> None
            | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for Children"))

    let (|Range|_|) (node : AstNode) = raise (new NotImplementedException())


    let GetChildren (node : AstNode) =
        match node with
            | Children(c) -> c
            | _ -> None
