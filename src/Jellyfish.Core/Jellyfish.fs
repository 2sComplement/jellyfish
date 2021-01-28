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

    /// Runs a single jellyfish in the provided tank and set of scented coordinates
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
    
    /// Runs a single jellyfish in the provided tank and set of scented coordinates and returns coordinate history
    let runSingleWithHistory tank scented jellyfish =
        jellyfish.Instructions
        |> List.fold (fun (positions, currentScented) i ->
            match positions with
            | [ Ok current ]
            | Ok current :: _ ->
                match Position.move tank currentScented current i with
                | Ok nextPosition -> 
                    Ok nextPosition :: positions, currentScented
                | Error p -> 
                    // Jellyfish moved out of the tank
                    Error p :: positions, currentScented |> Set.add (Position.coords p)

            // If the current position is not Ok _ then stop appending new positions
            | _ -> positions, currentScented
        ) ([ Ok jellyfish.InitialPosition ], scented)
        |> fun (history, scented) -> 
            // Reverse the list so the last position is at the end of the lists
            List.rev history, scented
            
    /// Runs all jellyfish in the provided tank
    let runAll tank jellies =
        jellies
        |> List.mapFold (runSingle tank) Set.empty
        
    /// Runs all jellyfish in the provided tank and returns coordinate history
    let runAllWithHistory tank jellies =
        jellies
        |> List.mapFold (runSingleWithHistory tank) Set.empty