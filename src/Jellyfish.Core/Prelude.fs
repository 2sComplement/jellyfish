namespace Jellyfish.Core

[<RequireQualifiedAccess>]
module Option =    
    let combine = function | Some a, Some b -> Some (a, b) | _ -> None

[<AutoOpen>]
module Common =
    open FSharpx

    /// Parses a set of coordinates from a string.
    ///
    /// Coordinates are parsed in any of the following formats: xy, xxy, xxyy
    let tryParseCoordinates (str: string) =
        let join = sprintf "%s%s"
        let cast (s: string) = System.Int32.TryParse s |> Option.ofBoolAndValue
        str
        |> Seq.map string
        |> Seq.toList
        |> function
            | [ x; y ] -> (cast x, cast y) |> Option.combine
            | [ x1; x2; y ] -> (cast <| join x1 x2, cast y) |> Option.combine
            | [ x1; x2; y1; y2 ]  -> (cast <| join x1 x2, cast <| join y1 y2) |> Option.combine
            | _ -> None