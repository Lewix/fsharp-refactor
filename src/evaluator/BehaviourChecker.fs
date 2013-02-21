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

let resultsBehaviourHasChanged entryFunction (afterResults : CompilerResults, afterAssembly) (beforeResults : CompilerResults, beforeAssembly) =
    match afterResults.Errors.HasErrors, beforeResults.Errors.HasErrors with
        | true, true -> false
        | false, false -> assemblyBehaviourHasChanged entryFunction beforeAssembly afterAssembly
        | _ -> true

let BehaviourHasChanged entryFunction (beforeSource : string) (afterSource : string) =
    resultsBehaviourHasChanged entryFunction (compile afterSource) (compile beforeSource)
