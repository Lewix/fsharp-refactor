module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis

type ScopeTree =
    | Declaration of (string * range) list * ScopeTree list
    | Usage of string * range

let rec getDeclarations p =
    match p with
        | CodeAnalysis.Declaration(text, range) -> [(text, range)]
        | Ast.Children cs -> List.concat (Seq.map getDeclarations cs)
        | _ -> []

//TODO: mutually recursive functions with and
let rec makeScopeTree (tree : Ast.AstNode) =
    match tree with
        | Ast.AstNode.Expression(SynExpr.LetOrUse(_,_,[b],e,_)) ->
            let l1,v =
                match b with
                    | SynBinding.Binding(_,_,_,_,_,_,_,p,_,e,_,_) ->
                        (makeScopeTree (Ast.AstNode.Expression e),
                         getDeclarations (Ast.AstNode.Pattern p))
            let l2 = makeScopeTree (Ast.AstNode.Expression e)
            Declaration(v,l2)::l1
        | Ast.Children(cs) -> List.concat (Seq.map makeScopeTree cs)
        | CodeAnalysis.Usage(text, range) -> [Usage(text, range)]
        | _ -> []
