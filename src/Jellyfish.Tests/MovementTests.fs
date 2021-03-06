module Jellyfish.MovementTests

open NUnit.Framework
open Jellyfish.Core
open Swensen.Unquote

[<Test>]
let ``Orientation can rotate correctly``() =

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
let ``Jellyfish instructions run with history given an initial position``() =
    
    let tank = { Width = 5; Height = 3 }
    let jellyfish =
        { InitialPosition = { X = 0; Y = 3; Orientation = W }
          Instructions = [ L; L; F; F ] }
    let scents = Set.empty

    let expectedPositions = 
        [ Ok { X = 0; Y = 3; Orientation = W } 
          Ok { X = 0; Y = 3; Orientation = S } 
          Ok { X = 0; Y = 3; Orientation = E } 
          Ok { X = 1; Y = 3; Orientation = E } 
          Ok { X = 2; Y = 3; Orientation = E } ]
    let actual = jellyfish |> Jellyfish.runSingleWithHistory tank scents
    actual =! (expectedPositions, scents)

[<Test>]
let ``Jellyfish instructions run out of bounds with history given instructions that go out of bounds``() =
    
    let tank = { Width = 5; Height = 3 }
    let jellyfish =
        { InitialPosition = { X = 0; Y = 3; Orientation = W }
          Instructions = [ L; L; L; F; F ] }
    let scents = Set.empty

    let expectedPositions = 
        [ Ok { X = 0; Y = 3; Orientation = W } 
          Ok { X = 0; Y = 3; Orientation = S } 
          Ok { X = 0; Y = 3; Orientation = E } 
          Ok { X = 0; Y = 3; Orientation = N } 
          Error { X = 0; Y = 3; Orientation = N } ]
    let actual = jellyfish |> Jellyfish.runSingleWithHistory tank scents
    actual =! (expectedPositions, set [ (0, 3) ])

[<Test>]
let ``Jellyfish instructions run out of bounds with history with scents``() =
    
    let tank = { Width = 5; Height = 3 }
    let jellyfish =
        { InitialPosition = { X = 0; Y = 3; Orientation = W }
          Instructions = [ F; L; L; R; F; F ] }
    let scents = set [ (0, 3) ]

    let expectedPositions = 
        [ Ok { X = 0; Y = 3; Orientation = W } 
          Ok { X = 0; Y = 3; Orientation = W } 
          Ok { X = 0; Y = 3; Orientation = S } 
          Ok { X = 0; Y = 3; Orientation = E } 
          Ok { X = 0; Y = 3; Orientation = S } 
          Ok { X = 0; Y = 2; Orientation = S } 
          Ok { X = 0; Y = 1; Orientation = S } ]
    let actual = jellyfish |> Jellyfish.runSingleWithHistory tank scents
    actual =! (expectedPositions, scents)