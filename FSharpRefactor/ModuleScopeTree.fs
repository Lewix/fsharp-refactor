module FSharpRefactor.Engine.ModuleScopeTree

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

type Module = string * range
type Declaration = string * range
type ModuleScopeTree = ScopeTree<Module * Declaration list, Declaration>

let nameFromLongIdent moduleIdentifier =
    String.concat "." (Seq.map (fun (i:Ident) -> i.idText) moduleIdentifier)

let rangeFromLongIdent (identifier:LongIdent) =
    let filename = (List.head identifier).idRange.FileName
    mkRange filename (List.head identifier).idRange.Start (Seq.last identifier).idRange.End

let declarationFromBinding prefix binding =
    let pattern =
        match binding with SynBinding.Binding(_,_,_,_,_,_,_,pattern,_,_,_,_) -> pattern
    match Ast.AstNode.Pattern pattern with
        | DeclaredIdent (name, range) -> Some (prefix + name, range)
        | _ -> None
       
let rec getDeclarations prefix declaration =
    match declaration with
        | SynModuleDecl.Let(_, bs, _) ->
            List.collect (declarationFromBinding prefix >> Option.toList) bs
        | SynModuleDecl.NestedModule(ComponentInfo(_,_,_,moduleIdentifier,_,_,_,_),ds,_,_) ->
            let moduleName = nameFromLongIdent moduleIdentifier
            List.collect (getDeclarations (prefix + moduleName + ".")) ds
        | _ -> []

let listUsedIdentifiers (node:Ast.AstNode) project =
    let rec usedIdentifiers (tree:IdentifierScopeTree) =
        match tree with
            | Usage(((n, r) as i, is)) ->
                let fullRange = mkRange r.FileName r.Start (i::is |> Seq.last |> snd).End
                [n::(List.map fst is), fullRange]
            | Declaration(_, ts) -> List.collect usedIdentifiers ts
    List.collect usedIdentifiers (makeScopeTrees node)

let rec getUsages declaredNames (project:Project) node =
    let usageRangeAndDeclarationLocation (names, usageRange:range) =
        let filename = usageRange.FileName
        let declarationLocation =
            Ast.TryGetDeclarationLocation project filename names (usageRange.StartLine, usageRange.StartColumn)
        if Option.isSome declarationLocation then Some (usageRange, declarationLocation.Value)
        else None
    let fullNameAndUsageRange (usageRange, ((line, col), filename)) =
        List.tryPick (fun (fullName, range) -> if rangeContainsPos range (mkPos line col) then Some (fullName, usageRange) else None)
                     declaredNames
    
    listUsedIdentifiers node project
    |> List.choose usageRangeAndDeclarationLocation
    |> List.choose fullNameAndUsageRange
    |> List.map Usage

let makeNestedScopeTrees followingFilesScope makeScopeTreesFunction nodes =
    let reversedNodes = List.rev nodes
    Seq.fold makeScopeTreesFunction followingFilesScope reversedNodes

let rec makeModuleScopeTreesWithPrefix declaredNames project prefix followingFilesScope (tree:Ast.AstNode) : ModuleScopeTree list =
    match tree with
        | Ast.AstNode.File(ParsedImplFileInput(_,_,_,_,_,ns,_)) ->
            makeNestedScopeTrees followingFilesScope (makeModuleScopeTreesWithPrefix declaredNames project prefix) (List.map Ast.AstNode.ModuleOrNamespace ns)
        | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace(namespaceIdentifier, false, declarations, _, _, _, _)) ->
            let namespaceName = nameFromLongIdent namespaceIdentifier
            makeNestedScopeTrees followingFilesScope (makeModuleScopeTreesWithPrefix declaredNames project (prefix + namespaceName + ".")) (List.map Ast.AstNode.ModuleDeclaration declarations)
        | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace(moduleIdentifier, true, declarations, _, _, _, _))
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.NestedModule(ComponentInfo(_,_,_,moduleIdentifier,_,_,_,_),declarations,_,_)) as m ->
            let moduleName = nameFromLongIdent moduleIdentifier
            let moduleRange = (Ast.GetRange m).Value
            let moduleDeclarations = List.collect (getDeclarations (prefix + moduleName + ".")) declarations
            let moduleUsages = getUsages declaredNames project tree
            Declaration(((prefix + moduleName, moduleRange), moduleDeclarations),followingFilesScope)::moduleUsages
        | Ast.Children cs -> List.collect (makeModuleScopeTreesWithPrefix declaredNames project prefix followingFilesScope) cs
        | _ -> followingFilesScope

let rec getModuleDeclarations prefix tree =
    match tree with
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.NestedModule(ComponentInfo(_,_,_,identifier,_,_,_,_),declarations,_,_))
        | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace(identifier, _, declarations, _, _, _, _)) ->
            let moduleName = nameFromLongIdent identifier
            List.collect (getModuleDeclarations (prefix + moduleName + ".")) (List.map Ast.AstNode.ModuleDeclaration declarations)
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(_,bs,_)) ->
            List.collect (declarationFromBinding prefix >> Option.toList) bs
        | Ast.Children cs -> List.collect (getModuleDeclarations prefix) cs
        | _ -> []

let rec makeModuleScopeTrees (project:Project) =
    let reversedFiles = Array.rev project.Files
    let fileContents = Array.map project.GetContents reversedFiles
    let trees =
        Seq.map (fun filename -> (Ast.Parse project filename).Value) reversedFiles
    let declaredNames = Seq.collect (getModuleDeclarations "") trees |> Seq.toList
    
    Seq.fold (makeModuleScopeTreesWithPrefix declaredNames project "") [] trees