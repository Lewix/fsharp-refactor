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

let rec getUsages node =
    match node with
        | Ast.AstNode.Expression(SynExpr.LongIdent(_,LongIdentWithDots(declarationIdentifier,_),_,_)) ->
            [Usage(nameFromLongIdent declarationIdentifier, rangeFromLongIdent declarationIdentifier)]
        | Ast.Children cs -> List.collect getUsages cs
        | _ -> []
        
let makeNestedScopeTrees followingFilesScope makeScopeTreesFunction nodes =
    let reversedNodes = List.rev nodes
    Seq.fold makeScopeTreesFunction followingFilesScope reversedNodes

let rec makeModuleScopeTreesWithPrefix prefix followingFilesScope (tree:Ast.AstNode) : ModuleScopeTree list =
    match tree with
        | Ast.AstNode.File(ParsedImplFileInput(_,_,_,_,_,ns,_)) ->
            makeNestedScopeTrees followingFilesScope (makeModuleScopeTreesWithPrefix prefix) (List.map Ast.AstNode.ModuleOrNamespace ns)
        | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace(namespaceIdentifier, false, declarations, _, _, _, _)) ->
            let namespaceName = nameFromLongIdent namespaceIdentifier
            makeNestedScopeTrees followingFilesScope (makeModuleScopeTreesWithPrefix (prefix + namespaceName + ".")) (List.map Ast.AstNode.ModuleDeclaration declarations)
        | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace(moduleIdentifier, true, declarations, _, _, _, _))
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.NestedModule(ComponentInfo(_,_,_,moduleIdentifier,_,_,_,_),declarations,_,_)) as m ->
            let moduleName = nameFromLongIdent moduleIdentifier
            let moduleRange = (Ast.GetRange m).Value
            let moduleDeclarations = List.collect (getDeclarations (prefix + moduleName + ".")) declarations
            let moduleUsages = List.collect getUsages (List.map Ast.AstNode.ModuleDeclaration declarations)
            Declaration(((prefix + moduleName, moduleRange), moduleDeclarations),followingFilesScope)::moduleUsages
        | Ast.Children cs -> List.collect (makeModuleScopeTreesWithPrefix prefix followingFilesScope) cs
        | _ -> followingFilesScope
        
let rec makeModuleScopeTrees (project:Project) =
    let reversedFiles = Array.rev project.Files
    let fileContents = Array.map project.GetContents reversedFiles
    
    Seq.zip fileContents reversedFiles
    |> Seq.map (fun (source,filename) -> (Ast.Parse source filename).Value)
    |> Seq.fold (makeModuleScopeTreesWithPrefix "") []