module FSharpRefactor.Engine.ReferenceFinder

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

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
        Ast.TryGetDeclarationLocation project range.FileName names (range.StartLine, range.StartColumn)
    let (_, filename) as declarationLocation =
        let locationIfNotDeclaration = tryGetDeclarationLocation(names, range)
        if Option.isSome locationIfNotDeclaration then locationIfNotDeclaration.Value
        else (range.StartLine, range.StartColumn), range.FileName
    
    project.FilesInScope filename
    |> Seq.map (Ast.Parse project >> Option.get)
    |> Seq.collect (makeProjectScopeTrees project)
    |> Seq.collect (findPotentialReferences names)
    //|> Seq.map tryGetDeclarationLocation
    |> Seq.filter (tryGetDeclarationLocation >> (=) (Some declarationLocation))
    
let FindDeclarationReferences (project:Project) (name, range) =
    let getRelevantName (names, range) =
        Seq.find ((=) name) names, range
    FindReferences project [name] range
    |> Seq.map getRelevantName