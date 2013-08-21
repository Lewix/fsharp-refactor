module FSharpRefactor.Engine.ReferenceFinder

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis
open FSharpRefactor.Engine.Projects
open FSharpRefactor.Engine.Modules

let declarationEscapesFile (project:Project) ((name, declarationRange):Identifier) =
    let tree = project.GetParseTree declarationRange.FileName
    
    let declaredInBinding binding =
        let ident = declarationFromBinding binding
        Some (name, declarationRange) = ident
    
    let declaredInBindings = (List.map declaredInBinding) >> (List.fold (||) false)
    
    let rec declarationEscapesFileInTree tree =
        match tree with
            | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(_, bs, _)) when declaredInBindings bs -> true
            | Ast.Children cs ->
                List.map declarationEscapesFileInTree cs
                |> List.fold (||) false
            | _ -> false
            
    declarationEscapesFileInTree tree
                

let rec findPotentialReferences names (tree:IdentifierScopeTree) =
    let relevantName = Seq.last names
    let hasRelevantName (n, _) = n = relevantName
    match tree with
        | Usage(i, is) ->
            if List.exists hasRelevantName (i::is) then
                let index = List.findIndex hasRelevantName (i::is)
                let names = List.map fst (i::is)
                [Seq.take (index+1) names, snd (Seq.nth index (i::is))]
            else []
        | Declaration(_,ts) -> List.collect (findPotentialReferences names) ts

let FindReferences (project:Project) names (range:range) =
    let tryGetDeclarationLocation (names, range:range) =
        TryGetDeclarationLocation project range.FileName names (range.StartLine, range.StartColumn)
    let (_, filename) as declarationLocation =
        let locationIfNotDeclaration = tryGetDeclarationLocation(names, range)
        if Option.isSome locationIfNotDeclaration then locationIfNotDeclaration.Value
        else (range.StartLine, range.StartColumn), range.FileName
    
    project.FilesInScope filename
    |> Seq.map (GetParseTree project)
    |> Seq.collect (makeProjectScopeTrees project)
    |> Seq.collect (findPotentialReferences names)
    |> Seq.filter (tryGetDeclarationLocation >> (=) (Some declarationLocation))
    
let FindDeclarationReferences (project:Project) (name, range) =
    let getRelevantName (names, range) =
        Seq.find ((=) name) names, range
    FindReferences project [name] range
    |> Seq.map getRelevantName
    
let FindDeclarationReferencesInFile identifierScope (name, range) =
    let rangeOfIdent (name : string) (identifiers : Identifier list) =
        let identifier = List.tryFind (fun (n,_) -> n = name) identifiers
        if Option.isNone identifier then None else Some(snd identifier.Value)
    let isNestedDeclaration idents =
        List.exists (fun (n,r) -> n = name && not (rangeContainsRange r range)) idents
    
    let rec findReferencesInTree tree =
        match tree with
            | Usage((n,r),_) when n = name -> [r]
            | Declaration(is, ts) when not (isNestedDeclaration is) ->
                List.concat (Seq.map findReferencesInTree ts)
            | _ -> []
    findReferencesInTree identifierScope
