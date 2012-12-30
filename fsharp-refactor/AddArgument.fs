module FSharpRefactor.Refactorings.AddArgument

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow

let AddArgumentToBinding source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) =
    refactoring source {
        let arguments =
            match FindBindingAtRange bindingRange tree with
                | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,
                                                         SynPat.LongIdent(_,_,_,args,_,_),_,_,_,_)) -> args
                | _ -> raise (new NotImplementedException "Binding did not have the right form")
        let firstArgRange = Ast.GetRange (Ast.AstNode.Pattern (List.head arguments))
        if Option.isSome firstArgRange then yield (firstArgRange.Value.StartRange, argumentName + " ")
    }

let AddArgumentToFunctionCall source (tree : Ast.AstNode) (callRange : range) (argument : string) =
    refactoring source {
        let rec findArg nodesAtCallRange =
            match nodesAtCallRange with
                | [] -> raise (new NotImplementedException "No app found")
                | Ast.AstNode.Expression(SynExpr.App(_,_,_,arg,_))::_ -> Ast.AstNode.Expression arg
                | e::es -> findArg es
        //TODO: GetRange shouldn't return an Option, it's a pain
        let argRange = (Ast.GetRange (findArg (FindNodesWithRange callRange tree))).Value
        yield (argRange.StartRange, argument + " ")
    }

let AddArgument source (bindingRange : range) (argumentName : string) (defaultValue : string) = source
