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

let AddArgumentToBinding (bindingRange : range) (argumentName : string) =
    let transform (source, ()) =
        let tree = (Ast.Parse source).Value
        let identEndRange =
            match FindBindingAtRange bindingRange tree with
                | SynBinding.Binding(_,_,_,_,_,_,_,
                                     SynPat.LongIdent(functionName,_,_,_,_,_),_,_,_,_) -> functionName.Range.EndRange
                | SynBinding.Binding(_,_,_,_,_,_,_,
                                     SynPat.Named(_,valueName,_,_,_),_,_,_,_) -> valueName.idRange.EndRange
                | b -> raise (RefactoringFailure("Binding did not have the right form:" + (sprintf "%A" b)))
        [identEndRange, " " + argumentName], ()

    { analysis = (fun (_,_) -> Valid); transform = transform }

//TODO: Add brackets around usage if needed (if it's not an App)
let AddArgumentToFunctionUsage (argument : string) (identRange : range) =
    { analysis = (fun (_,_) -> Valid); transform = fun (_,_) -> ([identRange.EndRange, " " + argument],()) }

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
                | _ -> raise (RefactoringFailure("Binding was not a function"))


let CanAddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    try findFunctionName source tree bindingRange |> ignore; Valid with
        | :? KeyNotFoundException -> Invalid("No binding found at the given range")
        | RefactoringFailure(m) -> Invalid(m)

//TODO: Check arguments such as argumentName or defaultValue have a valid form
let AddTempArgument doCheck source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    let analysis (source, ()) = CanAddArgument source (Ast.Parse source).Value bindingRange argumentName defaultValue
    let usageRefactorings =
        findFunctionName source tree bindingRange
        |> FindFunctionUsageRanges source tree bindingRange
        |> List.map (AddArgumentToFunctionUsage defaultValue)
    let bindingRefactoring = AddArgumentToBinding bindingRange argumentName
    { List.fold interleave bindingRefactoring usageRefactorings with analysis = analysis }

let AddArgument doCheck source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    let unusedName = FindUnusedName tree
    let sourceWithTempArgument = RunNewRefactoring (refactor (AddTempArgument doCheck source tree bindingRange unusedName defaultValue) () source)
    let tree = (Ast.Parse sourceWithTempArgument).Value
    let identifier = (TryFindIdentifierWithName (makeScopeTrees tree) unusedName).Value
    refactor (Rename doCheck identifier argumentName) () sourceWithTempArgument
    
let DoAddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    RunNewRefactoring (AddArgument true source tree bindingRange argumentName defaultValue)
