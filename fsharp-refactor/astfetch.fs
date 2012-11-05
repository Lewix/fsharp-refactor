open Microsoft.FSharp.Compiler.SourceCodeServices


let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))


let a = 2

printfn "%A" a
