namespace FSharpRefactor.Engine

open System 
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

module ASTFetcher =
    let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))
    let filename = "/home/lewis/test.fs"
    let options source = checker.GetCheckOptionsFromScriptRoot(filename, source, DateTime.Now, [| |])
    
    let Parse source = checker.UntypedParse(filename, source, options source).ParseTree

    let maxOfList ns = Seq.fold max 0 ns

    let rec patternHeight pattern =
        match pattern with
            | SynPat.Named(innerPattern,_,_,_,_) -> 1 + (patternHeight innerPattern)
            | SynPat.Wild(_) -> 0
            | _ -> raise (new NotImplementedException("This declaration is not supported"))
            
    let expressionHeight expression =
        match expression with
            | SynExpr.Const(_,_) -> 1
            | _ -> raise (new NotImplementedException("This declaration is not supported"))
            
    let bindingHeight (Binding(_,_,_,_,_,_,_,pattern,_,expression,_,_)) =
        1 + (maxOfList [(patternHeight pattern); (expressionHeight expression)])
    
    let declarationHeight declaration =
        match declaration with
            | SynModuleDecl.Let(_,bindings,_) ->
                let bindingHeights = Seq.map bindingHeight bindings
                1 + (maxOfList bindingHeights)
            | _ -> raise (new NotImplementedException("This declaration is not supported"))
        
    let moduleOrNamespaceHeight (SynModuleOrNamespace(_,_,declarations,_,_,_,_)) =
        let declarationHeights = Seq.map declarationHeight declarations
        1 + (maxOfList declarationHeights)

    let Height tree =
        match tree with
            | ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,modulesOrNamespaces,_)) ->
                let moduleHeights = Seq.map moduleOrNamespaceHeight modulesOrNamespaces
                maxOfList moduleHeights
            | _ -> raise (new NotImplementedException("Use an impl file instead"))

    
    let TextOfRange source range = ""
        
