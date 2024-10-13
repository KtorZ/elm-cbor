module Bech32.Internal exposing (..)

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Dict exposing (Dict)


checksumLength : Int
checksumLength =
    6


alphabet : Dict Char Int
alphabet =
    "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
        |> String.toList
        |> List.indexedMap (\i c -> ( c, i ))
        |> Dict.fromList


checksum : String -> Result Char Int
checksum prefix =
    prefix
        |> String.foldl
            (\c ->
                Result.andThen <|
                    \chk ->
                        let
                            i =
                                Char.toCode c
                        in
                        if i < 33 || i > 126 then
                            Err c

                        else
                            Ok <| Bitwise.xor (polymodStep chk) (shiftRightBy 5 i)
            )
            (Ok 1)
        |> Result.map
            (\chk0 ->
                String.foldl
                    (\c chk ->
                        Bitwise.xor (polymodStep chk) (and (Char.toCode c) 0x1F)
                    )
                    (polymodStep chk0)
                    prefix
            )


polymodStep : Int -> Int
polymodStep pre =
    let
        b =
            pre |> shiftRightBy 25
    in
    let
        step n magicNumber =
            Bitwise.xor (and -(and (shiftRightBy n b) 1) magicNumber)
    in
    shiftLeftBy 5 (and pre 0x01FFFFFF)
        |> step 0 0x3B6A57B2
        |> step 1 0x26508E6D
        |> step 2 0x1EA119FA
        |> step 3 0x3D4233DD
        |> step 4 0x2A1462B3


type EncodingDirection
    = Word8ToWord5
    | Word5ToWord8


type WordEncodingFailure
    = ExcessPadding
    | NonZeroPadding


encodeWords : EncodingDirection -> List Int -> Result WordEncodingFailure (List Int)
encodeWords dir wordsIn =
    let
        inBits =
            case dir of
                Word5ToWord8 ->
                    5

                Word8ToWord5 ->
                    8

        outBits =
            case dir of
                Word5ToWord8 ->
                    8

                Word8ToWord5 ->
                    5

        ( finalValue, finalBits, finalWords ) =
            List.foldl
                (\word ( value0, bits0, wordsOut0 ) ->
                    let
                        value =
                            value0
                                |> shiftLeftBy inBits
                                |> or word

                        ( wordsOut, bits ) =
                            nextWords value [] (bits0 + inBits) outBits
                    in
                    ( value, bits, wordsOut ++ wordsOut0 )
                )
                ( 0, 0, [] )
                wordsIn

        padding =
            finalValue
                |> shiftLeftBy (outBits - finalBits)
                |> and (maxValue outBits)
    in
    case dir of
        Word5ToWord8 ->
            if finalBits >= inBits then
                Err ExcessPadding

            else if padding /= 0 then
                Err NonZeroPadding

            else
                List.reverse finalWords |> Ok

        Word8ToWord5 ->
            (if finalBits > 0 then
                padding :: finalWords

             else
                finalWords
            )
                |> List.reverse
                |> Ok


maxValue : Int -> Int
maxValue outBits =
    shiftLeftBy outBits 1 - 1


nextWords : Int -> List Int -> Int -> Int -> ( List Int, Int )
nextWords value words bits0 outBits =
    if bits0 >= outBits then
        let
            bits =
                bits0 - outBits

            word =
                value |> shiftRightBy bits |> and (maxValue outBits)
        in
        nextWords value (word :: words) bits outBits

    else
        ( words, bits0 )
