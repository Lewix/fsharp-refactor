namespace FSharpRefactor.Engine

open System
open System.IO

type Project(currentFile:string, filesAndContents:(string * string option) array) as self =
    let currentFile = Path.GetFullPath currentFile
    let getIndex fileName = Seq.findIndex ((=) fileName) self.Files

    new(source:string, fileName:string) =
        Project(fileName, [|fileName, Some source|])

    member self.Files with get() = Array.map (fst >> Path.GetFullPath) filesAndContents
    member self.CurrentFile with get() = currentFile
    member self.FileContents = Array.map snd filesAndContents
    member self.CurrentFileContents = self.GetContents self.CurrentFile
    
    member self.FilesInScope fileName =
        let fileName = Path.GetFullPath fileName
        Seq.skipWhile ((<>) fileName) self.Files |> Seq.toList
    member self.GetContents fileName = 
        let fileName = Path.GetFullPath fileName
        let index = getIndex fileName
        if Option.isSome self.FileContents.[index] then self.FileContents.[index].Value
        else File.ReadAllText fileName
    member self.UpdateContents fileName contents =
        let fileName = Path.GetFullPath fileName
        let index = getIndex fileName
        let filesAndContents = Array.copy filesAndContents
        Array.set filesAndContents index (fileName, Some contents)
        new Project(currentFile, filesAndContents)
    member self.UpdateCurrentFileContents contents =
        self.UpdateContents self.CurrentFile contents