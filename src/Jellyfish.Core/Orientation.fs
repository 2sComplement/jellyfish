namespace Jellyfish.Core

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
