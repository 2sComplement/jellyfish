module Jellyfish.Console.Program

open System
open Jellyfish.Core
open Argu

type ParsedInput = Result<(Tank * Jellyfish list), string>

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
    
    let isTankInvalid tank = 
        tank.Width < 0
        || tank.Height < 0
        || tank.Width > MaxWidth 
        || tank.Height > MaxHeight

[<AutoOpen>]
module Helpers =

    let private parseTank str =

        match str |> Tank.tryParse with
        | None -> Error "Invalid tank specified"
        | Some tank when Validation.isTankInvalid tank -> Error (sprintf "Tank size must be between 0x0 and %dx%d" Validation.MaxWidth Validation.MaxHeight)
        | Some tank -> Ok tank

    let private loadFile inputFile =
        if IO.File.Exists inputFile
        then IO.File.ReadAllLines inputFile |> Ok
        else Error "File does not exist"
            
    let getInputFromConsole () : ParsedInput = 

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

    let getInputFromFile inputFile : ParsedInput =

        let printFileContents input =
            input |> Array.iter (printfn "%s")
            input
            
        let processInput input =
            match List.ofArray input with
            | t :: instr ->
                parseTank t
                |> Result.map (fun tank -> tank, instr |> List.choose Jellyfish.tryParse)
            | _ -> Error "File was not formatted correctly"

        printfn "Loading instructions from %s:" inputFile
        loadFile inputFile |> Result.bind (printFileContents >> processInput)

[<EntryPoint>]
let main argv =

    let newline () = printfn ""
    let space () = printf " "
    let printResult =
        function
        | Ok p -> printf "%O" p
        | Error p -> printf "%OLOST" p //)
            
    // Arguments parsing
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "Jellyfish.Console.exe", errorHandler = errorHandler)
    let results = parser.Parse argv
    let showHistory = results.Contains History

    match results.TryGetResult File with 
    | Some inputFile -> getInputFromFile inputFile
    | None -> getInputFromConsole ()
    |> function
        | Error msg -> printfn "Error: %s" msg
        | Ok (tank, jellyfish) when showHistory ->
            printfn "\nOutput (with history):"
            Jellyfish.runAllWithHistory tank jellyfish
            |> fst
            |> List.iter (fun history ->
                history |> List.tryLast |> Option.iter printResult
                printf " - "
                history |> List.iter (printResult >> space)
            )

        | Ok (tank, jellyfish) ->
            printfn "\nOutput:"
            Jellyfish.runAll tank jellyfish
            |> fst
            |> List.iter (printResult >> newline)


    printfn "\n\nPress enter to exit"
    Console.ReadLine() |> ignore
    0 // return an integer exit code