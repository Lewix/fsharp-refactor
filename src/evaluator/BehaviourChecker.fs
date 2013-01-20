module FSharpRefactor.Evaluator.BehaviourChecker

open System.Text
open System.IO
open System.Reflection
open System.CodeDom.Compiler
open Microsoft.FSharp.Compiler.CodeDom

// Get beforeSource:
// let f a = a in f arg
//
// Add some stuff around it:
//
// module beforeModule
//
// let before arg =
//     let f a = a in f arg

let defaultModule = "Evaluator"

let loadFunction assemblyPath moduleName methodName : (int -> int) =
    let assembly = Assembly.LoadFrom(assemblyPath)
    let methodClosure arg =
        assembly.GetModule(assemblyPath).GetType(moduleName).GetMethod(methodName).Invoke(null,[|arg|]) :?> int
    methodClosure

let padSource (source : string) =
    "module " + defaultModule + "\n\n" + source

let functionBehaviourHasChanged before after =
    Seq.init 100000 (fun i -> i)
    |> Seq.map (fun i -> before i <> after i)
    |> Seq.fold (||) false

let assemblyBehaviourHasChanged entryFunction beforePath afterPath =
    let before = loadFunction beforePath defaultModule entryFunction
    let after = loadFunction afterPath defaultModule entryFunction
    functionBehaviourHasChanged before after

let compile source =
    let codeProvider = new FSharpCodeProvider()
    let path = Path.GetTempFileName() + ".dll"

    let parameters = new CompilerParameters()
    parameters.OutputAssembly <- path
    path, codeProvider.CompileAssemblyFromSource(parameters, padSource source)

let BehaviourHasChanged entryFunction (beforeSource : string) (afterSource : string) =
    let beforeAssembly, results1 = compile beforeSource
    let afterAssembly, results2 = compile afterSource

    match results1.Errors.HasErrors, results2.Errors.HasErrors with
        | true, true -> false
        | false, false -> assemblyBehaviourHasChanged entryFunction beforeAssembly afterAssembly
        | _ -> true
