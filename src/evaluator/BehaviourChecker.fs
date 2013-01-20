module FSharpRefactor.Evaluator.BehaviourChecker

open System.Reflection
open System.CodeDom.Compiler
open Microsoft.FSharp.Compiler.CodeDom

let BehaviourHasChanged argumentName beforeSource afterSource =
    true
