module FSharpRefactor.Refactorings.AddArgument

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.Rename

let DefaultBindingRange source (tree : Ast.AstNode) (position : pos) =
    let range = mkRange "test.fs" position position
    let rec tryFindDeepestBinding trees =
        let candidateBinding = List.tryPick (TryFindBindingAroundRange range) trees
        if Option.isNone candidateBinding then None
        else
            let children = Ast.GetChildren (Ast.AstNode.Binding candidateBinding.Value)
            if Option.isNone children then candidateBinding
            else
                let nestedBinding =
                    tryFindDeepestBinding (Ast.GetChildren (Ast.AstNode.Binding candidateBinding.Value)).Value
                if Option.isNone nestedBinding then candidateBinding else nestedBinding

    let deepestBinding = tryFindDeepestBinding [tree]
    if Option.isNone deepestBinding then None
    else Ast.GetRange (Ast.AstNode.Binding deepestBinding.Value)

let AddArgumentToBinding source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) =
    refactoring source Valid {
        let identEndRange =
            match FindBindingAtRange bindingRange tree with
                | SynBinding.Binding(_,_,_,_,_,_,_,
                                     SynPat.LongIdent(functionName,_,_,_,_,_),_,_,_,_) -> functionName.Range.EndRange
                | SynBinding.Binding(_,_,_,_,_,_,_,
                                     SynPat.Named(_,valueName,_,_,_),_,_,_,_) -> valueName.idRange.EndRange
                | b -> raise (new Exception("Binding did not have the right form:" + (sprintf "%A" b)))
        yield (identEndRange, " " + argumentName)
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
        | SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_) ->
            match Ast.AstNode.Pattern p with
                | DeclaredIdent(i,r) -> i
                | _ -> raise (new Exception("Binding was not a function"))


let CanAddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    try findFunctionName source tree bindingRange |> ignore; Valid with
        | :? KeyNotFoundException -> Invalid("No binding found at the given range")
        | e -> Invalid(e.Message)

//TODO: Check arguments such as argumentName or defaultValue have a valid form
let AddTempArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    let valid = CanAddArgument source tree bindingRange argumentName defaultValue
    refactoring source valid {
        let identRanges =
            findFunctionName source tree bindingRange
            |> FindFunctionUsageRanges source tree bindingRange
        yield! (AddArgumentToBinding source tree bindingRange argumentName)
        for identRange in identRanges do
            yield! (AddArgumentToFunctionUsage source tree identRange defaultValue)
    }

let AddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    let unusedName = FindUnusedName tree
    let sourceWithTempArgument = RunRefactoring (AddTempArgument source tree bindingRange unusedName defaultValue)
    let tree = (Ast.Parse sourceWithTempArgument).Value
    let identifier = (TryFindIdentifierWithName (makeScopeTrees tree) unusedName).Value
    Rename sourceWithTempArgument tree identifier argumentName

let DoAddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    RunRefactoring (AddArgument source tree bindingRange argumentName defaultValue)
