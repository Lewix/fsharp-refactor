module FSharpRefactor.Engine.ModuleScopeTree

open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

type Module = string * range
type Declaration = string * range
type ModuleScopeTree = ScopeTree<Module * (Declaration list), Module * Declaration>

let makeModuleScopeTrees (tree:Ast.AstNode) = []