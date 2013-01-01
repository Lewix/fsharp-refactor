module FSharpRefactor.Refactorings.AddArgument

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
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

let FindFunctionCalls source (tree : Ast.AstNode) (bindingRange : range) (functionName : string) =
    let isDeclarationOfFunction scopeTree =
        match scopeTree with
            | Declaration(is,ts) ->
                List.exists (fun (n,r) -> rangeContainsRange bindingRange r && n = functionName) is
            | _ -> false
    let isApp node =
        match node with
            | Ast.AstNode.Expression(SynExpr.App(_,_,_,_,_)) -> true
            | _ -> false
    let rangeIfUsageOfFunction scopeTree =
        match scopeTree with
            | Usage(n,r) -> if n = functionName then Some r else None
            | _ -> None
            
    let findAppAroundRange (functionRange : range) =
        let rec tryFindAppAroundRange tree =
            match tree with
                | Ast.AstNode.Expression(SynExpr.App(_,_,f,e,r) as app) ->
                    if rangeContainsRange r functionRange then
                        let nestedApp = List.tryPick tryFindAppAroundRange
                                                     [Ast.AstNode.Expression e; Ast.AstNode.Expression f]
                        if Option.isSome nestedApp then nestedApp else Some app
                    else None
                | Ast.Children cs -> List.tryPick tryFindAppAroundRange cs
                | _ -> None
        let app = tryFindAppAroundRange tree
        if Option.isSome app then app.Value
        else raise (new Collections.Generic.KeyNotFoundException("Not such node"))
    
    makeScopeTrees tree
    |> List.collect ListNodes
    |> List.find isDeclarationOfFunction
    |> ListNodes
    |> List.choose rangeIfUsageOfFunction
    |> List.map findAppAroundRange
    |> List.map Ast.AstNode.Expression
    
    
let AddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) = source
