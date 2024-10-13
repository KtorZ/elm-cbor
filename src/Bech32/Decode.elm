module Bech32.Decode exposing (DecodeFailure(..), decode)

import Bech32.Internal exposing (WordsToBytesFailure, checksum, checksumLength, polymodStep, wordsToBytes)
import Bitwise
import Bytes exposing (Bytes)
import Dict exposing (Dict)


type DecodeFailure
    = DataPayloadTooShort { minimum : Int, currentLength : Int }
    | UnexpectedCharacterInPayload { culprit : Char }
    | UnexpectedCharacterInPrefix { culprit : Char }
    | InvalidChecksum
    | TooManySeparators
    | InternalError WordsToBytesFailure


alphabet : Dict Char Int
alphabet =
    Bech32.Internal.alphabet
        |> String.toList
        |> List.indexedMap (\i c -> ( c, i ))
        |> Dict.fromList


decode : String -> Result DecodeFailure { prefix : String, data : Bytes }
decode rawInput =
    case String.split "1" (String.toLower rawInput) of
        [] ->
            Err <| DataPayloadTooShort { minimum = checksumLength, currentLength = 0 }

        [ _ ] ->
            Err <| DataPayloadTooShort { minimum = checksumLength, currentLength = 0 }

        [ prefix, payload ] ->
            let
                payloadLen =
                    String.length payload
            in
            if payloadLen < checksumLength then
                Err <| DataPayloadTooShort { minimum = checksumLength, currentLength = payloadLen }

            else
                checksum prefix
                    |> Result.mapError
                        (\c ->
                            UnexpectedCharacterInPrefix { culprit = c }
                        )
                    |> Result.andThen
                        (\chk0 ->
                            String.foldl
                                (\c ->
                                    Result.andThen
                                        (\( chk, words, cursor ) ->
                                            case Dict.get c alphabet of
                                                Nothing ->
                                                    Err <| UnexpectedCharacterInPayload { culprit = c }

                                                Just w ->
                                                    let
                                                        isChecksum =
                                                            cursor <= 0

                                                        newChk =
                                                            Bitwise.xor (polymodStep chk) w
                                                    in
                                                    Ok <|
                                                        if isChecksum then
                                                            ( newChk, words, cursor )

                                                        else
                                                            ( newChk, w :: words, cursor - 1 )
                                        )
                                )
                                (Ok ( chk0, [], String.length payload - checksumLength ))
                                payload
                        )
                    |> Result.andThen
                        (\( chk, words, _ ) ->
                            if chk /= 1 then
                                Err InvalidChecksum

                            else
                                words
                                    |> List.reverse
                                    |> wordsToBytes
                                    |> Result.mapError InternalError
                        )
                    |> Result.map
                        (\data ->
                            { prefix = prefix
                            , data = data
                            }
                        )

        _ ->
            Err TooManySeparators
