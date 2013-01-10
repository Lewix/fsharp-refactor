module FSharpRefactor.Refactorings.AddArgument

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow

let DefaultBindingRange source (tree : Ast.AstNode) (position : pos) =
    let range = mkRange "/home/lewis/test.fs" position position
    let rec tryFindDeepestBinding trees =
        let candidateBinding = List.tryPick (TryFindBindingAroundRange range) trees
        if Option.isNone candidateBinding then None
        else
            let children = Ast.GetChildren candidateBinding.Value
            if Option.isNone children then candidateBinding
            else
                let nestedBinding = tryFindDeepestBinding (Ast.GetChildren candidateBinding.Value).Value
                if Option.isNone nestedBinding then candidateBinding else nestedBinding

    let deepestBinding = tryFindDeepestBinding [tree]
    if Option.isNone deepestBinding then None
    else Ast.GetRange deepestBinding.Value

//TODO: Check this can add an argument to a value
let AddArgumentToBinding source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) =
    refactoring source Valid {
        let arguments =
            match FindBindingAtRange bindingRange tree with
                | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,
                                                         SynPat.LongIdent(_,_,_,args,_,_),_,_,_,_)) -> args
        
                | b -> raise (new NotImplementedException("Binding did not have the right form:" + (sprintf "%A" b)))
        let firstArgRange = Ast.GetRange (Ast.AstNode.Pattern (List.head arguments))
        if Option.isSome firstArgRange then yield (firstArgRange.Value.StartRange, argumentName + " ")
    }

//TODO: Add brackets around usage if needed (if it's not an App)
let AddArgumentToFunctionUsage source (tree : Ast.AstNode) (identRange : range) (argument : string) =
    refactoring source Valid {
        yield (identRange.EndRange, " " + argument)
    }

let FindFunctionUsageRanges source (tree : Ast.AstNode) (bindingRange : range) (functionName : string) =
    let isDeclarationOfFunction scopeTree =
        match scopeTree with
            | Declaration(is,ts) ->
                List.exists (fun (n,r) -> rangeContainsRange bindingRange r && n = functionName) is
            | _ -> false
    let rangeIfUsageOfFunction scopeTree =
        match scopeTree with
            | Usage(n,r) -> if n = functionName then Some r else None
            | _ -> None
            
    makeScopeTrees tree
    |> List.collect ListNodes
    |> List.find isDeclarationOfFunction
    |> ListNodes
    |> List.choose rangeIfUsageOfFunction

let findFunctionName source (tree : Ast.AstNode) (bindingRange : range) =
    match FindBindingAtRange bindingRange tree with
        | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_)) ->
            match Ast.AstNode.Pattern p with
                | DeclaredIdent(i,r) -> i
                | _ -> raise (new Exception("Binding was not a function"))
        | _ -> raise (new Exception("No binding at that range"))


//TODO: Check arguments such as argumentName or defaultValue have a valid form
let AddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    RunRefactoring (refactoring source Valid {
        let identRanges =
            findFunctionName source tree bindingRange
            |> FindFunctionUsageRanges source tree bindingRange
        yield! (AddArgumentToBinding source tree bindingRange argumentName)
        for identRange in identRanges do
            yield! (AddArgumentToFunctionUsage source tree identRange defaultValue)
    })
    
