module Main exposing (..)

import Html exposing (Html)



---- Day 1 ----


parseInput : String -> List Int
parseInput =
    String.lines >> List.filterMap (String.trim >> String.toInt)


sumRollingTriples : List Int -> List Int
sumRollingTriples numbers =
    List.map3 (\a b c -> a + b + c)
        numbers
        (List.drop 1 numbers)
        (List.drop 2 numbers)


countIncreases : List Int -> Int
countIncreases numbers =
    List.map2 (\a b -> b - a)
        numbers
        (List.drop 1 numbers)
        |> List.filter (\a -> a > 0)
        |> List.length


day1Part1 : String -> Int
day1Part1 =
    parseInput >> countIncreases


day1Part2 : String -> Int
day1Part2 =
    parseInput >> sumRollingTriples >> countIncreases


testData : String
testData =
    """
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
    """


testDay1 : Bool
testDay1 =
    day1Part1 testData == 7 && day1Part2 testData == 5


main : Html msg
main =
    Html.text <| Debug.toString testDay1
