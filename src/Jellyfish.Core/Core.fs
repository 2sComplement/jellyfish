namespace Jellyfish.Core

open System
open FSharpx

[<AutoOpen>]
module Common =

    let tryParseCoordinates : string -> (int * int) option =
        let join = sprintf "%s%s"
        let cast : string -> int option = System.Int32.TryParse >> Option.ofBoolAndValue
        Seq.map string
        >> Seq.toList
        >> function
            | [ w; h ] -> (cast w, cast h) |> Option.combine
            | [ w1; w2; h ] -> (cast <| join w1 w2, cast h) |> Option.combine
            | [ w1; w2; h1; h2 ]  -> (cast <| join w1 w2, cast <| join h1 h2) |> Option.combine
            | _ -> None

type Instruction = | L | R | F

[<RequireQualifiedAccess>]
module Instruction =
    
    let tryParse =
        function
        | "L" -> Some L
        | "R" -> Some R
        | "F" -> Some F
        | _ -> None
    
    let tryParseMany =        
        Seq.map (string >> tryParse)
        >> List.ofSeq
        >> Option.sequence
        

type Tank = { Width: int; Height: int }

[<RequireQualifiedAccess>]
module Tank =

    let create (w, h) = { Width = w; Height = h }
    
    let tryParse : string -> Tank option = tryParseCoordinates >> Option.map create

    let isOutOfBounds (x, y) tank = x < 0 || y < 0 || x > tank.Width || y > tank.Height

type Orientation = | N | E | S | W
    
[<RequireQualifiedAccess>]
module Orientation =
    
    let tryParse =
        function
        | "N" -> Some N
        | "E" -> Some E
        | "S" -> Some S
        | "W" -> Some W
        | _ -> None

    /// Rotates an orientation based on an instruction
    let rotate =
        let orientations = [ N; E; S; W ]
        let orientationIndexMap = orientations |> List.indexed |> List.map (fun (i, o) -> o, i) |> Map.ofList 
        fun instruction currentOrientation ->
            let i = orientationIndexMap |> Map.find currentOrientation
            match instruction with
            | L when i = 0 -> orientationIndexMap.Count - 1
            | L -> i - 1
            | R when i = orientationIndexMap.Count - 1 -> 0
            | R -> i + 1
            | F -> i
            |> fun i -> orientations.[i]

type Position =
    { X: int
      Y: int 
      Orientation: Orientation }
    override x.ToString() = sprintf "%d%d%O" x.X x.Y x.Orientation

[<RequireQualifiedAccess>]
module Position =

    let create ((x, y), o) = { X = x; Y = y; Orientation = o }

    let tryParse (str: string) =
        if str.Length > 0 then
            str 
            |> Seq.map string 
            |> Seq.toArray 
            |> Array.splitAt (str.Length - 1)
            |> fun (xy, o) -> 
                xy |> String.concat "" |> tryParseCoordinates,
                o |> Array.exactlyOne |> Orientation.tryParse
            |> Option.combine
            |> Option.map create
        else None

    /// Returns a tuple representing a position's x,y coordinates
    let coords position = position.X, position.Y

    let translate instruction currentPosition =
        match instruction, currentPosition.Orientation with
        | F, N ->  { currentPosition with Y = currentPosition.Y + 1 }
        | F, E -> { currentPosition with X = currentPosition.X + 1 }
        | F, S -> { currentPosition with Y = currentPosition.Y - 1 }
        | F, W -> { currentPosition with X = currentPosition.X - 1 }
        | (L | R), _ -> currentPosition

    /// Rotates a position based on an instruction
    let rotate instruction current =
        { current with Orientation = current.Orientation |> Orientation.rotate instruction }

    /// Moves a position based on the given tank size and scented coordinates.
    /// A move consists of a rotation and a translation.
    ///
    /// Returns `Ok newPosition` on a successful move.
    /// Returns `Error lastPosition` if the next position is outside the tank's bounds.
    let move tank scented currentPosition instruction =
        
        let nextPosition = 
            currentPosition 
            |> rotate instruction
            |> translate instruction

        let xy = coords nextPosition

        match Tank.isOutOfBounds xy tank with
        | false -> 
            Ok nextPosition
        | true when scented |> Set.contains (coords currentPosition) -> 
            Ok currentPosition // Jellies cannot fall off the tank from scented coordinates
        | true -> 
            Error currentPosition

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