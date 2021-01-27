namespace Jellyfish.Core

open System
open FSharpx

type Jellyfish =
    { InitialPosition: Position
      Instructions: Instruction list }
        
[<RequireQualifiedAccess>]
module Jellyfish =

    let create (ip, is) = { InitialPosition = ip; Instructions = is }

    let tryParse (str: string) =
        match str.Split(" ") |> Array.filter (not << String.IsNullOrWhiteSpace) with
        | [| initialPosition; instructions |] ->
            (initialPosition |> Position.tryParse, instructions |> Instruction.tryParseMany |> Option.map List.ofSeq)
            |> Option.combine
            |> Option.map create
        | _ -> None
    
    let runSingle tank scented jellyfish =
        jellyfish.Instructions
        |> List.fold (fun (currentPosition, currentScented) i ->
            match currentPosition with
            | Ok cp ->
                match Position.move tank currentScented cp i with
                | Ok nextPosition -> 
                    Ok nextPosition, currentScented
                | Error p -> 
                    // Jellyfish moved out of the tank
                    Error p, currentScented |> Set.add (Position.coords p)
            | Error p -> Error p, currentScented
        ) (Ok jellyfish.InitialPosition, scented)

    let runAll tank jellies =
        jellies
        |> List.mapFold (runSingle tank) Set.empty