namespace FSharpRefactor.Engine

open System 
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

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

    
    let TextOfRange (source : string) (range : range) =
        let lines = source.Split('\n')
        let startLine = lines.[range.StartLine-1].[range.StartColumn..]
        let endLine = lines.[range.EndLine-1].[range.StartColumn..]
        let rec getLines line =
            if line < range.EndLine-1 then lines.[line]::(getLines (line+1))
            else if line = range.EndLine-1 then [endLine]
            else [] // StartLine and EndLine are equal

        startLine::(getLines range.StartLine)
        |> Seq.fold (+) ""

        
