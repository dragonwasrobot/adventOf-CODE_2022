module Day20 exposing (solve)

import Array exposing (Array)
import Array.Extra as Array
import Maybe.Extra as Maybe
import Tuple


type alias Entry =
    { value : Int
    , originalIndex : Int
    , moved : Bool
    }


solve : String -> ( Int, Int )
solve rawInput =
    let
        parsedArray : Array Int
        parsedArray =
            parseArray rawInput

        answer1 : Int
        answer1 =
            parsedArray
                |> initArray 1
                |> mixArray 1
                |> computeGrooveCoordinates

        answer2 : Int
        answer2 =
            parsedArray
                |> initArray 811589153
                |> mixArray 10
                |> computeGrooveCoordinates
    in
    -- ( 2203, 6641234038999 )
    ( Debug.log "Answer 1" answer1, Debug.log "Answer 2" answer2 )


parseArray : String -> Array Int
parseArray =
    String.split "\n"
        >> List.map String.toInt
        >> Maybe.values
        >> Array.fromList


initArray : Int -> Array Int -> Array Entry
initArray decryptionKey =
    Array.foldl (\e ( acc, i ) -> ( Entry (e * decryptionKey) i False :: acc, i + 1 )) ( [], 0 )
        >> Tuple.first
        >> Array.fromList
        >> Array.reverse


mixArray : Int -> Array Entry -> Array Entry
mixArray rounds array =
    let
        doRound mixingArray round =
            if round == 0 then
                mixingArray

            else
                let
                    newMixingArray =
                        array
                            |> Array.foldl (\entry accArray -> moveEntry accArray entry) mixingArray
                            |> Array.map (\entry -> { entry | moved = False })
                in
                doRound newMixingArray (round - 1)
    in
    doRound array rounds


moveEntry : Array Entry -> Entry -> Array Entry
moveEntry array entry =
    let
        optCurrentIndex =
            findCurrentIndex array entry.originalIndex
    in
    case optCurrentIndex of
        Nothing ->
            array

        Just currentIdx ->
            let
                tmpArray =
                    Array.removeAt currentIdx array

                newIdx =
                    computeNewIndex (Array.length tmpArray) currentIdx entry.value

                newEntry =
                    { entry | moved = True }
            in
            Array.insertAt newIdx newEntry tmpArray


findCurrentIndex : Array Entry -> Int -> Maybe Int
findCurrentIndex array idx =
    array |> findIndex (\e -> e.originalIndex == idx)


findIndex : (a -> Bool) -> Array a -> Maybe Int
findIndex predicate array =
    let
        length =
            Array.length array

        doIterate currentIdx =
            if currentIdx >= length then
                Nothing

            else
                let
                    foundElement =
                        array
                            |> Array.get currentIdx
                            |> Maybe.unwrap False predicate
                in
                if foundElement then
                    Just currentIdx

                else
                    doIterate (currentIdx + 1)
    in
    doIterate 0


computeNewIndex : Int -> Int -> Int -> Int
computeNewIndex arrayLength currentIdx value =
    let
        newIdx =
            remainderBy arrayLength (currentIdx + value)
    in
    if newIdx < 0 then
        arrayLength + newIdx

    else
        newIdx


computeGrooveCoordinates : Array Entry -> Int
computeGrooveCoordinates array =
    let
        zeroIdx =
            array
                |> findIndex (\e -> e.value == 0)
                |> Maybe.withDefault 0

        firstNumberIdx =
            computeNewIndex (Array.length array) zeroIdx 1000

        secondNumberIdx =
            computeNewIndex (Array.length array) zeroIdx 2000

        thirdNumberIdx =
            computeNewIndex (Array.length array) zeroIdx 3000

        firstNumber =
            array
                |> Array.get firstNumberIdx
                |> Maybe.unwrap 0 .value

        secondNumber =
            array
                |> Array.get secondNumberIdx
                |> Maybe.unwrap 0 .value

        thirdNumber =
            array
                |> Array.get thirdNumberIdx
                |> Maybe.unwrap 0 .value
    in
    firstNumber + secondNumber + thirdNumber
