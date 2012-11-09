namespace FSharpRefactor.Engine.Ast

open System
open Microsoft.FSharp.Compiler.Ast

module Ast =
    // Active patterns to make dealing with the syntax tree more convenient
    let (|ModuleOrNamespaceChildren|_|) (expression : SynModuleOrNamespace) =
        match expression with
            | SynModuleOrNamespace (_,_,e1,_,_,_,_) -> Some(e1)

        
    let (|SynExprChildren|_|) (expression : SynExpr) = function
        | SynExpr.Paren(e1,_,_,_) -> Some([e1])
        | SynExpr.Quote(e1,_,e2,_,_) -> Some([e1;e2])
        | _ -> raise (new NotImplementedException("Add a new entry to the active pattern for SynExpr children"))
