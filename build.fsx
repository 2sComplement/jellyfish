#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    !! "src/*.sln"
    |> Seq.iter (DotNet.build id)
)

Target.create "Publish" (fun _ ->
    "src/Jellyfish.Console/Jellyfish.Console.fsproj"
    |> sprintf "-r win-x64 -c Release -o bin /p:PublishSingleFile=true %s"
    |> DotNet.exec id "publish" 
    |> ignore
)

Target.create "Test" (fun _ ->
    for testFile in !! "src/**/bin/**/*.Tests.dll" do
        match DotNet.exec id "vstest" testFile with
        | result when result.ExitCode <> 0 -> failwith "Process failed"
        | _ -> ()
)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"
