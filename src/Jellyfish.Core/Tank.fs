namespace Jellyfish.Core

open FSharpx

type Tank = { Width: int; Height: int }

[<RequireQualifiedAccess>]
module Tank =

    let create (w, h) = { Width = w; Height = h }
    
    let tryParse : string -> Tank option = tryParseCoordinates >> Option.map create

    let isOutOfBounds (x, y) tank = x < 0 || y < 0 || x > tank.Width || y > tank.Height