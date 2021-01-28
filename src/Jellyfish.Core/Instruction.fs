namespace Jellyfish.Core

open FSharpx

type Instruction = | L | R | F

[<RequireQualifiedAccess>]
module Instruction =
    
    let tryParse =
        function
        | "L" -> Some L
        | "R" -> Some R
        | "F" -> Some F
        | _ -> None
    
    let tryParseMany (str: string) =        
        str 
        |> Seq.map (string >> tryParse)
        |> List.ofSeq
        |> Option.sequence
        