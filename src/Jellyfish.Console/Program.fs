module Jellyfish.Console.Program

open System
open Jellyfish.Core

[<AutoOpen>]
module Helpers =

    let fmtResult finalPositions =
        printfn "\nOutput:"
        finalPositions
        |> List.map (
            function
            | Ok p -> sprintf "%O" p
            | Error p -> sprintf "%OLOST" p)

    let processInput input = ()

[<EntryPoint>]
let main argv =

    printfn "Enter instructions followed by a blank line when finished"

    match Console.ReadLine() |> Tank.tryParse with
    | None -> printfn "Invalid tank specified"
    | Some tank when Tank.isInvalid tank -> printfn "Tank cannot be larger than %d x %d" Tank.MaxWidth Tank.MaxHeight
    | Some tank ->

        let rec getJellyfish acc =
            let instruction = Console.ReadLine()
            if String.IsNullOrWhiteSpace instruction
            then acc
            else [ instruction ] |> List.append acc |> getJellyfish

        let input = getJellyfish []

        let jellyfish = input |> List.choose Jellyfish.tryParse
        
        Jellyfish.runAll tank jellyfish
        |> fst
        |> fmtResult
        |> List.iter (printfn "%s")

    0 // return an integer exit code