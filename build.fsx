#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
// open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

Target.create "Test" (fun _ ->
    for testFile in !! "src/**/bin/**/*.Tests.dll" do
        match DotNet.exec id "vstest" testFile with
        | result when result.ExitCode <> 0 -> failwith "Process failed"
        | _ -> ()


    // |> NUnit3.run (fun p ->
    //    { p with
    //         ShadowCopy = false })
 )

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "All"

Target.runOrDefault "All"
