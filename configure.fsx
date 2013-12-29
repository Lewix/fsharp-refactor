open System
open System.IO

let GetPath (str: string) =
  let separatedPath = str.Replace('/', Path.DirectorySeparatorChar)
  Path.GetFullPath (String.Join (Path.DirectorySeparatorChar.ToString (),
                                 separatedPath))

let FileReplace (file, outFile, toReplace:string, replacement:string) =
  File.WriteAllText (GetPath outFile, File.ReadAllText(GetPath file).Replace(toReplace, replacement))

let isWindows = (Path.DirectorySeparatorChar = '\\')

let prefix = "/usr/local"
let mono = "/usr/bin/mono"
let version = "0.1.0.0"

// Put version in AssemblyInfo in both projects
FileReplace("FSharpRefactor/AssemblyInfo.fs.in", "FSharpRefactor/AssemblyInfo.fs",
            "@VERSION@", version)
FileReplace("FSharpRefactor.CommandLine/AssemblyInfo.fs.in",
            "FSharpRefactor.CommandLine/AssemblyInfo.fs",
            "@VERSION@", version)


if not isWindows then
  // Make the binary for the command-line tool
  FileReplace("bin/fsharp-refactor.in", "bin/fsharp-refactor", "@MONO@", mono)

  // Update prefix in Makefile
  FileReplace("Makefile.in", "Makefile", "@prefix@", prefix)
