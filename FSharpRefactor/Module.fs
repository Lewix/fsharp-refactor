namespace FSharpRefactor.Engine

open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast

type Module =
    {
        fullName : string * string list
        declarations : string list
        nestedModules : Module list
        filename : string
    }
    
module Modules =
    let nameFromLongIdent (moduleIdentifier:LongIdent) =
        (List.head moduleIdentifier).idText, List.map (fun (i:Ident) -> i.idText) (List.tail moduleIdentifier)

    let (|DeclaredIdent|_|) (node : Ast.AstNode) =
        match node with
            | Ast.Pattern(SynPat.Named(_,i,_,_,_)) -> Some(i.idText, i.idRange)
            | Ast.Pattern(SynPat.LongIdent(LongIdentWithDots([_;_],_),_,_,_,_,_)) ->
                // This is a member declaration of the form self.id, ignore both idents
                None
            | Ast.Pattern(SynPat.LongIdent(LongIdentWithDots(i::_,_),_,_,_,_,_)) ->
                Some(i.idText, i.idRange)
            | Ast.SimplePattern(SynSimplePat.Id(i,_,_,_,_,_)) ->
                Some(i.idText, i.idRange)
            | _ -> None

    let declarationFromBinding binding =
        let pattern =
            match binding with SynBinding.Binding(_,_,_,_,_,_,_,pattern,_,_,_,_) -> pattern
        match Ast.AstNode.Pattern pattern with
            | DeclaredIdent (name, _) -> Some name
            | _ -> None
           

    let rec addNamespace (i, is) m =
        let n, ns = m.fullName
        { m with fullName = i, is @ (n::ns); nestedModules = List.map (addNamespace (i, is)) m.nestedModules }
        
    let addNestedModules nm m =
        { m with nestedModules = m.nestedModules @ nm }
        
    let addModuleDeclarations ds m =
        { m with declarations = m.declarations @ ds }

    let (|Namespace|_|) (node:Ast.AstNode) =
        match node with
            | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(ident, false, _, _, _, _, _)) ->
                let children = Ast.GetChildren node
                Some (nameFromLongIdent ident, children.Value)
            | _ -> None

    let (|DeclaredModule|_|) (node:Ast.AstNode) =
        match node with                
            | Ast.AstNode.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(ident, true, declarations, _, _, _, range))
            | Ast.AstNode.ModuleDeclaration(SynModuleDecl.NestedModule(ComponentInfo(_,_,_,ident,_,_,_,_), declarations, _, range)) ->
                Some (
                    {
                        fullName = nameFromLongIdent ident;
                        declarations = [];
                        nestedModules = [];
                        filename = range.FileName
                    }, (Ast.GetChildren node).Value)
            | _ -> None
            
    let (|ModuleDeclarations|_|) (node:Ast.AstNode) =
        match node with
            | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(_, bs, _)) ->
                Some (List.collect (declarationFromBinding >> Option.toList) bs)
            | _ -> None


    let GetModules (project:Project) =        
        let rec getModuleDeclarations = function
            | Namespace _ | DeclaredModule _ -> []
            | ModuleDeclarations ds -> ds
            | Ast.Children cs -> List.collect getModuleDeclarations cs
            | _ -> []
        let rec getDeclaredModules = function
            | Namespace (namespaceName, ds) ->
                List.collect getDeclaredModules ds
                |> List.map (addNamespace namespaceName)
            | DeclaredModule (m, ds) ->
                let nestedModules =
                    List.collect getDeclaredModules ds
                    |> List.map (addNamespace m.fullName)
                let moduleDeclarations = List.collect getModuleDeclarations ds
                [addNestedModules nestedModules m
                |> addModuleDeclarations moduleDeclarations]
            | Ast.Children cs -> List.collect getDeclaredModules cs
            | _ -> []
        
        Seq.map (Ast.Parse project >> Option.get) project.Files
        |> Seq.collect getDeclaredModules