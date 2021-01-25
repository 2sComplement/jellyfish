module Jellyfish.Tests

open NUnit.Framework
open Jellyfish.Core
open Swensen.Unquote

[<Test>]
let ``Orientation can rotate properly``() =

    let testRotation (initialOrientation, instruction, expectedOrientation) =
        let actualOrientation = initialOrientation |> Orientation.rotate instruction
        actualOrientation =! expectedOrientation

    [ (N, L, W) 
      (N, R, E)
      (W, L, S)
      (W, R, N)
      (S, L, E)
      (S, R, W)
      (E, L, N)
      (E, R, S) ]
    |> List.iter testRotation

[<Test>]
let ``Position can translate properly``() =

    let testTranslation (initialPosition, instruction, expectedPosition) = 
        let actualPosition = initialPosition |> Position.translate instruction
        actualPosition =! expectedPosition

    let mkPositions ((x, y), o, i, (x', y')) = 
        { X = x; Y = y; Orientation = o }, // initial
        i,
        { X = x'; Y = y'; Orientation = o } // expected

    [ (0,0), N, F, (0,1)
      (1,2), E, F, (2,2)
      (2,4), S, F, (2,3)
      (3,6), W, F, (2,6)
      (4,8), N, L, (4,8) 
      (4,8), N, R, (4,8) 
      (5,10), E, L, (5,10)
      (5,10), E, R, (5,10)
      (6,12), S, L, (6,12)
      (6,12), S, R, (6,12)
      (7,14), W, L, (7,14)
      (7,14), W, R, (7,14) ]
    |> List.iter (mkPositions >> testTranslation)

let private testMoveIn3x3Tank mkExpected currentPosition = 
    
    //let currentPosition = { X = 2; Y = 2; Orientation = orientation }
    let tank = { Height = 3; Width = 3 }
    let scented = Set.empty
    let instruction = F
    let expected = mkExpected currentPosition // { currentPosition with X = currentPosition.X + 1 }
    let nextPosition = Position.move tank scented currentPosition instruction

    nextPosition =! expected

[<Test>]
let ``Position can move within tank bounds in X direction``() =

    { X = 2; Y = 2; Orientation = E }
    |> testMoveIn3x3Tank (fun p -> Ok { p with X = 3 })

[<Test>]
let ``Position can move within tank bounds in -X direction``() =

    { X = 2; Y = 2; Orientation = W }
    |> testMoveIn3x3Tank (fun p -> Ok { p with X = 1 })

[<Test>]
let ``Position can move within tank bounds in Y direction``() =
    
    { X = 2; Y = 2; Orientation = N }
    |> testMoveIn3x3Tank (fun p -> Ok { p with Y = 3 })

[<Test>]
let ``Position can move within tank bounds in -Y direction``() =
    
    { X = 2; Y = 2; Orientation = S }
    |> testMoveIn3x3Tank (fun p -> Ok { p with Y = 1 })

[<Test>]
let ``Position is lost when moving out of tank bounds in X direction``() =
    
    { X = 3; Y = 3; Orientation = E }
    |> testMoveIn3x3Tank Error

[<Test>]
let ``Position is lost when moving out of tank bounds in -X direction``() =
    
    { X = 0; Y = 3; Orientation = W }
    |> testMoveIn3x3Tank Error

[<Test>]
let ``Position is lost when moving out of tank bounds in Y direction``() =
    
    { X = 0; Y = 3; Orientation = N }
    |> testMoveIn3x3Tank Error

[<Test>]
let ``Position is lost when moving out of tank bounds in -Y direction``() =
    
    { X = 3; Y = 0; Orientation = S }
    |> testMoveIn3x3Tank Error

[<Test>]
let ``Position is parsed given valid input``() =

    Position.tryParse "12N" =! Some { X = 1; Y = 2; Orientation = N }
    Position.tryParse "123E" =! Some { X = 12; Y = 3; Orientation = E }
    Position.tryParse "1234S" =! Some { X = 12; Y = 34; Orientation = S }
    Position.tryParse "00W" =! Some { X = 0; Y = 0; Orientation = W }

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

[<Test>]
let ``Jellyfish instructions run given an initial position with no scents``() =
    
    // 11E RFRFRFRF -> 11E

    let tank = { Width = 5; Height = 3 }
    let jellyfish =
        { InitialPosition = { X = 1; Y = 1; Orientation = E }
          Instructions = [ R; F; R; F; R; F; R; F ] }
    
    let expectedPosition = { X = 1; Y = 1; Orientation = E }
    let actual = jellyfish |> Jellyfish.runSingle tank Set.empty 
    actual =! (Ok expectedPosition, Set.empty)

[<Test>]
let ``Jellyfish instructions run given an initial position with scents``() =

    // 03W LLFFFLFLFL -> 23S
    
    let tank = { Width = 5; Height = 3 }
    let jellyfish =
        { InitialPosition = { X = 0; Y = 3; Orientation = W }
          Instructions = [ L; L; F; F; F; L; F; L; F; L ] }
    let scents = set [ (3, 3) ]

    let expectedPosition = { X = 2; Y = 3; Orientation = S }
    let actual = jellyfish |> Jellyfish.runSingle tank scents
    actual =! (Ok expectedPosition, scents)

[<Test>]
let ``Jellyfish instructions run given instructions that go out of bounds``() =

    // 32N FRRFLLFFRRFLL -> 33NLOST
    
    let tank = { Width = 5; Height = 3 }
    let jellyfish =
        { InitialPosition = { X = 3; Y = 2; Orientation = N }
          Instructions = [ F; R; R; F; L; L; F; F; R; R; F; L; L ] }
    let scents = Set.empty

    let expectedPosition = { X = 3; Y = 3; Orientation = N }
    let actual = jellyfish |> Jellyfish.runSingle tank scents
    actual =! (Error expectedPosition, set [ (3,3) ])