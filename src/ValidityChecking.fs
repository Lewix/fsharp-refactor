module FSharpRefactor.Engine.ValidityChecking

open Microsoft.FSharp.Compiler.Range

let IsSuccessful (check:'a->'b Option) (argument:'a Option) =
    if Option.isNone argument then lazy None
    else lazy (check argument.Value)
let PairOptions (x, y) = 
    match x, y with
        | Some a, Some b -> Some(a,b)
        | _ -> None
let Andalso (message1:Lazy<string option>) message2 =
    if Option.isSome (message1.Force()) then message1
    else message2

let PosFromPositionOption position =
    match position with
        | Some (line,col) -> Some (mkPos line (col-1))
        | None -> None
