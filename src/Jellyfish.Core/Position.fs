namespace Jellyfish.Core

open FSharpx

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
