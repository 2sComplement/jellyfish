module Jellyfish.Console.Program

open System
open Jellyfish.Core
open Argu

type ParsedResult = Result<(Tank * Jellyfish list), string>

type Arguments =
    | [<AltCommandLine("-f")>] File of string
    | [<AltCommandLine("-h")>] History
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | File _ -> "Specify an instructions file"
            | History -> "Output a history of jellyfish positions"

module Validation =

    [<Literal>]
    let MaxHeight = 60

    [<Literal>]
    let MaxWidth = 60
    
    let isTankInvalid tank = tank.Width > MaxWidth || tank.Height > MaxHeight

[<AutoOpen>]
module Helpers =

    let fmtResult finalPositions =
        printfn "\nOutput:"
        finalPositions
        |> List.map (
            function
            | Ok p -> sprintf "%O" p
            | Error p -> sprintf "%OLOST" p)

    let private parseTank str =

        match str |> Tank.tryParse with
        | None -> Error "Invalid tank specified"
        | Some tank when Validation.isTankInvalid tank -> Error (sprintf "Tank cannot be larger than %d x %d" Validation.MaxWidth Validation.MaxHeight)
        | Some tank -> Ok tank

    let private loadFile inputFile =
        if IO.File.Exists inputFile
        then IO.File.ReadAllLines inputFile |> Ok
        else Error "File does not exist"
            
    let getInputFromConsole () : ParsedResult = 

        printfn "Enter instructions followed by a blank line when finished"

        Console.ReadLine()
        |> parseTank
        |> Result.map (fun tank ->
            let rec getJellyfish acc =
                let instruction = Console.ReadLine()
                if String.IsNullOrWhiteSpace instruction
                then acc
                else [ instruction ] |> List.append acc |> getJellyfish

            let jellyfish = getJellyfish [] |> List.choose Jellyfish.tryParse
            (tank, jellyfish)
        )

    let getInputFromFile inputFile : ParsedResult =

        printfn "Loading instructions from %s" inputFile

        loadFile inputFile
        |> Result.bind (fun input ->
            match List.ofArray input with
            | t :: instr ->
                parseTank t
                |> Result.map (fun tank ->
                    tank, instr |> List.choose Jellyfish.tryParse
                )
            | _ -> Error "File was not formatted correctly"
        )

[<EntryPoint>]
let main argv =

    let parser = ArgumentParser.Create<Arguments>(programName = "Jellyfish.Console.exe")
    let results = parser.Parse argv

    match results.TryGetResult File with 
    | Some inputFile -> getInputFromFile inputFile
    | None -> getInputFromConsole ()
    |> function
        | Error msg -> printfn "Error: %s" msg
        | Ok (tank, jellyfish) ->

            Jellyfish.runAll tank jellyfish
            |> fst
            |> fmtResult
            |> List.iter (printfn "%s")

    0 // return an integer exit code