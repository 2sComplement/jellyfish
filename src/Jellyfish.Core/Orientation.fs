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
        
        // All orientations in circular order
        let orientations = [ N; E; S; W ] 

        // Map each orientation's index using the orientation as the key
        let orientationIndexMap = orientations |> List.indexed |> List.map (fun (i, o) -> o, i) |> Map.ofList 

        fun instruction currentOrientation ->
            let i = orientationIndexMap |> Map.find currentOrientation
            match instruction with
            | L when i = 0 -> orientationIndexMap.Count - 1 // Going left from the first element returns the last element
            | L -> i - 1
            | R when i = orientationIndexMap.Count - 1 -> 0 // Going right from the last element returns the first element
            | R -> i + 1
            | F -> i
            |> fun i -> orientations.[i]
