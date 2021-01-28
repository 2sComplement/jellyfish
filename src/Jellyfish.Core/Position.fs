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
        
        if str.Length >= 3 then

            let lastIndex = str.Length - 1
            let xy = str.Substring(0, lastIndex)
            let orientation = str.Substring(lastIndex)

            (xy |> tryParseCoordinates, orientation |> Orientation.tryParse)
            |> Option.combine
            |> Option.map create

        else None

    let coords position = position.X, position.Y

    /// Translates a position based on an instruction
    let translate instruction currentPosition =
        match instruction, currentPosition.Orientation with
        | F, N ->  { currentPosition with Y = currentPosition.Y + 1 }
        | F, E -> { currentPosition with X = currentPosition.X + 1 }
        | F, S -> { currentPosition with Y = currentPosition.Y - 1 }
        | F, W -> { currentPosition with X = currentPosition.X - 1 }
        | (L | R), _ -> currentPosition // Rotation does not translate the position

    /// Rotates a position based on an instruction
    let rotate instruction current =
        { current with Orientation = current.Orientation |> Orientation.rotate instruction }

    /// Moves a position based on the given tank size and scented coordinates.
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
            Ok currentPosition // Jellyfish cannot fall off the tank from scented coordinates
        | true -> 
            Error currentPosition
