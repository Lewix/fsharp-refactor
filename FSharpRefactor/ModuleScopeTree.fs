module FSharpRefactor.Engine.ModuleScopeTree

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

type Module = string * range
type Declaration = string * range
type ModuleScopeTree = ScopeTree<Module * (Declaration list), Module * Declaration>

let moduleFromLongIdent moduleIdentifier =
    let moduleName = String.concat "." (Seq.map (fun (i:Ident) -> i.idText) moduleIdentifier)
    let filename = (List.head moduleIdentifier).idRange.FileName
    let moduleRange = mkRange filename (List.head moduleIdentifier).idRange.Start (Seq.last moduleIdentifier).idRange.End
    (moduleName, moduleRange)

let declarationFromBinding binding =
    let pattern =
        match binding with SynBinding.Binding(_,_,_,_,_,_,_,pattern,_,_,_,_) -> pattern
    match Ast.AstNode.Pattern pattern with
        | DeclaredIdent identifier -> Some identifier
        | _ -> None

let getDeclarations declarations =
    match declarations with
        | SynModuleDecl.Let(_, bs, _) ->
            List.collect (declarationFromBinding >> Option.toList) bs
        | _ -> []
    
let rec makeModuleScopeTrees (tree:Ast.AstNode) : ModuleScopeTree list =
    match tree with
        | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace(moduleIdentifier, true, declarations, _, _, _, _)) ->
            let moduleDeclarations = List.collect getDeclarations declarations
            [Declaration((moduleFromLongIdent moduleIdentifier, moduleDeclarations),[])]
            //TODO: nested modules
        | Ast.Children cs -> List.collect makeModuleScopeTrees cs
        | _ -> []