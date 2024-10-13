module Bech32.Encode exposing (encode)

import Array exposing (Array)
import Bech32.Internal exposing (alphabet, bytesToWords, checksum, polymodStep)
import Bitwise exposing (and, shiftRightBy)
import Bytes exposing (Bytes)


type EncodeFailure
    = UnexpectedCharacterInPrefix { culprit : Char }
    | InvalidBytes


alphabet : Array Char
alphabet =
    Bech32.Internal.alphabet
        |> String.toList
        |> Array.fromList


unsafeCharFrom : Int -> String
unsafeCharFrom i =
    Array.get i alphabet
        |> Maybe.map String.fromChar
        |> Maybe.withDefault "!impossible!"


encode : { prefix : String, data : Bytes } -> Result EncodeFailure String
encode { prefix, data } =
    checksum (String.toLower prefix)
        |> Result.mapError
            (\c ->
                UnexpectedCharacterInPrefix { culprit = c }
            )
        |> Result.andThen
            (\chk0 ->
                case bytesToWords data of
                    Nothing ->
                        Err InvalidBytes

                    Just words ->
                        Ok <|
                            List.foldl
                                (\word ( str, chk ) ->
                                    ( String.append str (unsafeCharFrom word)
                                    , Bitwise.xor (polymodStep chk) word
                                    )
                                )
                                ( String.append prefix "1", chk0 )
                                words
            )
        |> Result.map
            (\( str, chk ) ->
                str ++ suffix chk
            )


suffix : Int -> String
suffix chk0 =
    let
        chk =
            List.foldl (\_ -> polymodStep) chk0 (List.range 0 5) |> Bitwise.xor 1
    in
    List.foldl
        (\k str ->
            let
                i =
                    chk |> shiftRightBy ((5 - k) * 5) |> and 0x1F
            in
            String.append str (unsafeCharFrom i)
        )
        ""
        (List.range 0 5)
