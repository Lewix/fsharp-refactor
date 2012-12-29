module FSharpRefactor.Refactorings.AddArgument

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow

let rec last list =
    match list with
        | [] -> raise (new ArgumentException "The input list was empty")
        | [l] -> l
        | l::ls -> last ls

let AddArgumentToBinding source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) =
    refactoring source {
        let arguments =
            match FindBindingAtRange bindingRange tree with
                | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,
                                                         SynPat.LongIdent(_,_,_,args,_,_),_,_,_,_)) -> args
                | _ -> raise (new NotImplementedException "Binding did not have the right form")
        let range = Ast.GetRange (Ast.AstNode.Pattern (last arguments))
        if Option.isSome range then yield (range.Value.EndRange, " " + argumentName)
    }
    

let AddArgument source (bindingRange : range) (argumentName : string) (defaultValue : string) = source
