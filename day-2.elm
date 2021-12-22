module Main exposing (..)

import Html exposing (Html)
import List.Extra exposing (scanl)
import Maybe exposing (andThen)


parseInput : String -> List String
parseInput =
    String.trim >> String.lines


parseMove : String -> String -> Int
parseMove direction input =
    if String.contains direction input then
        input
            |> String.split " "
            |> List.reverse
            |> List.head
            |> andThen String.toInt
            |> Maybe.withDefault 0

    else
        0


moves : String -> List String -> List Int
moves direction =
    List.map (parseMove direction)


movesForward : List String -> List Int
movesForward =
    moves "forward"


movesUp : List String -> List Int
movesUp =
    moves "up"


movesDown : List String -> List Int
movesDown =
    moves "down"


calcPosition1 : List String -> Int
calcPosition1 inputs =
    let
        horizontalPos : Int
        horizontalPos =
            movesForward inputs |> List.sum

        verticalPos : Int
        verticalPos =
            List.map2 (-)
                (movesDown inputs)
                (movesUp inputs)
                |> List.sum
    in
    horizontalPos * verticalPos


calcPosition2 : List String -> Int
calcPosition2 inputs =
    let
        cumSum : List Int -> List Int
        cumSum =
            scanl (+) 0

        horizontalMoves : List Int
        horizontalMoves =
            movesForward inputs

        aim : List Int
        aim =
            List.map2 (-)
                (movesDown inputs)
                (movesUp inputs)
                |> cumSum

        verticalPos : Int
        verticalPos =
            List.map2 (*) horizontalMoves aim
                |> List.sum
    in
    List.sum horizontalMoves * verticalPos


day2Part1 : String -> Int
day2Part1 =
    parseInput >> calcPosition1


day2Part2 : String -> Int
day2Part2 =
    parseInput >> calcPosition2


testData : String
testData =
    """
    forward 5
    down 5
    forward 8
    up 3
    down 8
    forward 2
    """


testDay2 : Bool
testDay2 =
    day2Part1 testData == 150 && day2Part2 testData == 900


main : Html msg
main =
    Html.text <| Debug.toString testDay2
