module FSharpRefactor.Refactorings.AddArgument

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow

let AddArgumentToBinding source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) =
    refactoring source true {
        let arguments =
            match FindBindingAtRange bindingRange tree with
                | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,
                                                         SynPat.LongIdent(_,_,_,args,_,_),_,_,_,_)) -> args
                | _ -> raise (new NotImplementedException "Binding did not have the right form")
        let firstArgRange = Ast.GetRange (Ast.AstNode.Pattern (List.head arguments))
        if Option.isSome firstArgRange then yield (firstArgRange.Value.StartRange, argumentName + " ")
    }

let AddArgumentToFunctionCall source (tree : Ast.AstNode) (callRange : range) (argument : string) =
    refactoring source true {
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
            
    let tryFindAppForRange (functionRange : range) =
        let rec tryFindAppForRange tree =
            match tree with
                | Ast.AstNode.Expression(SynExpr.App(_,_,SynExpr.Ident(id),e,_) as app) ->
                    if id.idRange = functionRange then Some app
                    else if rangeContainsRange (Ast.GetRange (Ast.AstNode.Expression e)).Value functionRange
                    then List.tryPick tryFindAppForRange [Ast.AstNode.Expression e]
                    else None
                | Ast.Children cs -> List.tryPick tryFindAppForRange cs
                | _ -> None
        tryFindAppForRange tree
    
    makeScopeTrees tree
    |> List.collect ListNodes
    |> List.find isDeclarationOfFunction
    |> ListNodes
    |> List.choose rangeIfUsageOfFunction
    |> List.choose tryFindAppForRange
    |> List.map Ast.AstNode.Expression

let findFunctionName source (tree : Ast.AstNode) (bindingRange : range) =
    match FindBindingAtRange bindingRange tree with
        | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_)) ->
            match Ast.AstNode.Pattern p with
                | DeclaredIdent(i,r) -> i
                | _ -> raise (new Exception("Binding was not a function"))
        | _ -> raise (new Exception("No binding at that range"))

        
let AddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    RunRefactoring (refactoring source true {
        let callRanges =
            findFunctionName source tree bindingRange
            |> FindFunctionCalls source tree bindingRange
            |> List.map Ast.GetRange
            |> List.map Option.get
        yield! (AddArgumentToBinding source tree bindingRange argumentName)
        for callRange in callRanges do
            yield! (AddArgumentToFunctionCall source tree callRange defaultValue)
    })
    
