module FSharpRefactor.Evaluator.BehaviourChecker

open System.Text
open System.IO
open System.Reflection
open System.CodeDom.Compiler
open Microsoft.FSharp.Compiler.CodeDom
open FSharpRefactor.Evaluator.CodeRefactorer

// Get beforeSource:
// let f (a:int) = a in f arg
//
// Add some stuff around it:
//
// module Evaluator
//
// let f (a:int) = a in f arg

let defaultModule = "Evaluator"

let padSource (source : string) =
    "module " + defaultModule + "\n\n" + source

let compile source =
    let codeProvider = new FSharpCodeProvider()
    let path = Path.GetTempFileName() + ".dll"

    let parameters = new CompilerParameters()
    parameters.OutputAssembly <- path
    codeProvider.CompileAssemblyFromSource(parameters, padSource source), path

let loadFunction assemblyPath moduleName methodName : (int -> int) =
    let assembly = Assembly.LoadFrom(assemblyPath)
    let methodObject =
        assembly.GetModule(Path.GetFileName(assemblyPath))
                .GetType(moduleName)
                .GetMethod(methodName)
    if methodObject.IsGenericMethod then raise CouldNotRefactor
    else (fun arg -> methodObject.Invoke(null,[|arg|]) :?> int)

let functionBehaviourHasChanged before after =
    Seq.init 100000 (fun i -> i)
    |> Seq.map (fun i -> before i <> after i)
    |> Seq.fold (||) false

let assemblyBehaviourHasChanged entryFunction beforePath afterPath =
    let before = loadFunction beforePath defaultModule entryFunction
    let after = loadFunction afterPath defaultModule entryFunction
    functionBehaviourHasChanged before after

type BehaviourChange =
    | Changed of bool
    | Unchanged of bool

let BehaviourHasChanged entryFunction (beforeSource : string) (afterSource : string) =
    let beforeResults, beforeAssembly = compile beforeSource
    let afterResults, afterAssembly = compile afterSource
    let changed =
        match afterResults.Errors.HasErrors, beforeResults.Errors.HasErrors with
            | false, false -> assemblyBehaviourHasChanged entryFunction beforeAssembly afterAssembly
            | true, true -> false
            | _ -> true
    not beforeResults.Errors.HasErrors, not afterResults.Errors.HasErrors, changed
