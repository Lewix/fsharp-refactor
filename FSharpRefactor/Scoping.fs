module FSharpRefactor.Engine.Scoping

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis
open FSharpRefactor.Engine.RangeAnalysis

let tryFindIdentifierDeclaration (trees : IdentifierScopeTree list) ((name, range) : Identifier) =
    let isDeclaration (n,r) = n = name && r = range
    let isSameName (n,r) = n = name
        
    let rec tryFindIdentifierAndDeclaration previousDeclaration tree =
        match tree with
            | Usage((n,r),_) -> if n = name && r = range then previousDeclaration else None
            | Declaration(is, ts) ->
                if List.exists isDeclaration is then
                    List.tryFind isDeclaration is
                elif List.exists isSameName is then
                    List.tryPick (tryFindIdentifierAndDeclaration (List.tryFind isSameName is)) ts
                else
                    List.tryPick (tryFindIdentifierAndDeclaration previousDeclaration) ts

    List.tryPick (tryFindIdentifierAndDeclaration None) trees

let rec tryFindDeclarationScope trees (name, declarationRange) =
    match trees with
        | [] -> None
        | Usage(_,_)::ds -> tryFindDeclarationScope ds (name, declarationRange)
        | (Declaration(is, ts) as d)::ds ->
            let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
            if List.exists isDeclaration is then Some d
            else tryFindDeclarationScope (List.append ts ds) (name, declarationRange)
            
let getDeclarations (trees : IdentifierScopeTree list) =
    let rec declarationsInSingleTree tree =
        match tree with
            | Usage(n,_) -> Set []
            | Declaration(is, ts) ->
                let declarationsInChildren =
                    Set.unionMany (Seq.map declarationsInSingleTree ts)
                Set.union declarationsInChildren (Set(List.map fst is))

    Set.unionMany (Seq.map declarationsInSingleTree trees)

let FindUnusedName project (tree : Ast.AstNode) =
    let scopeTrees = makeProjectScopeTrees project tree
    let usedNames = getDeclarations scopeTrees
    let randomNumberGenerator = new System.Random()

    let rec generateWhileUsed () =
        let name = "tmpFunction" + string (randomNumberGenerator.Next())
        if Set.contains name usedNames then generateWhileUsed ()
        else name

    generateWhileUsed ()

let TryGetIdentifierScope (project:Project) ((i, is):Identifier * Identifier list) =
    let _, range = i
    let names = (fst i)::(List.map fst is)
    let declarationLocation =
        Ast.TryGetDeclarationLocation project range.FileName names (range.StartLine, range.StartColumn)
    let scopeTrees = makeProjectScopeTrees project (Ast.Parse project range.FileName).Value
    let identifierDeclaration =
        Option.bind (fun ((line, col), filename) -> TryFindIdentifier project filename (mkPos line col)) declarationLocation
        |> Option.bind (fst >> tryFindIdentifierDeclaration scopeTrees)
    identifierDeclaration
    |> Option.bind (tryFindDeclarationScope scopeTrees)
    |> Option.map (fun scope -> new IdentifierScope(identifierDeclaration.Value, scope, project))
    
let GetIdentifierScope project identifier =
    TryGetIdentifierScope project identifier
    |> Option.get