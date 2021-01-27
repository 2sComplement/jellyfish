module Jellyfish.ParsingTests

open NUnit.Framework
open Jellyfish.Core
open Swensen.Unquote

[<Test>]
let ``Coordinates are parsed correctly given valid input``() =
    
    let test (input, expected) =
        tryParseCoordinates input =! Some expected

    [ "12", (1, 2)
      "123", (12, 3)
      "1234", (12, 34)
      " 1 2", (1, 2) ]
    |> List.iter test

[<Test>]
let ``Coordinates are not parsed given invalid input``() =
    let test input =
        tryParseCoordinates input =! None
    
    [ ""
      "1"
      " 1 23"
      "12345" ]
    |> List.iter test

[<Test>]
let ``Orientations are parsed correctly given valid input``() =
    
    let test (input, expected) = 
        Orientation.tryParse input =! Some expected

    [ "N", N
      "E", E
      "S", S
      "W", W ]
    |> List.iter test
    
[<Test>]
let ``Orientations are not parsed given invalid input``() =

    let test input = 
        Orientation.tryParse input =! None
    
    [ ""
      "NE"
      "n"
      " E" ]
    |> List.iter test

[<Test>]
let ``Position is parsed given valid input``() =

    let test (input, expected) = 
        Position.tryParse input =! Some expected
        
    [ "12N", { X = 1; Y = 2; Orientation = N }
      "123E", { X = 12; Y = 3; Orientation = E }
      "1234S", { X = 12; Y = 34; Orientation = S }
      "00W", { X = 0; Y = 0; Orientation = W } ]
    |> List.iter test

[<Test>]
let ``Position is not parsed given invalid input``() =

    let test input =
        let actual = input |> Position.tryParse
        actual =! None

    test ""
    test " "
    test "12 N"
    test "12A"
    test "12345E"    

[<Test>]
let ``Tank is parsed given valid input``() =

    let test input expected =
        let actual = input |> Tank.tryParse
        actual =! Some expected

    test "12" { Width = 1; Height = 2 }
    test "123" { Width = 12; Height = 3 }
    test "1234" { Width = 12; Height = 34 }

[<Test>]
let ``Tank is not parsed given invalid input``() =

    let test input =
        let actual = input |> Tank.tryParse
        actual =! None
        
    test ""
    test "a12"
    test "12314124124"

[<Test>]
let ``Instructions are parsed given valid input``() =
    
    let test input expected =
        let actual = input |> Instruction.tryParse
        actual =! Some expected

    test "R" R
    test "L" L
    test "F" F

[<Test>]
let ``Instructions are not parsed given invalid input``() =
    
    let test input =
        let actual = input |> Instruction.tryParse
        actual =! None

    [ 'A' .. 'Z' ]
    |> List.except [ 'R'; 'L'; 'F' ]
    |> List.iter (string >> test)
