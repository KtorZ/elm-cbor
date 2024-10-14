module Bech32.Decode exposing (DecodeFailure(..), decode)

import Bech32.Internal exposing (WordsToBytesFailure, checksum, checksumLength, polymodStep, wordsToBytes)
import Bitwise
import Bytes exposing (Bytes)
import Dict exposing (Dict)


type DecodeFailure
    = DataPayloadTooShort { minimum : Int, currentLength : Int }
    | PrefixTooShort { minimum : Int, currentLength : Int }
    | UnexpectedCharacterInPayload { culprit : Char }
    | UnexpectedCharacterInPrefix { culprit : Char }
    | InvalidChecksum
    | MissingSeparator
    | InternalError WordsToBytesFailure


alphabet : Dict Char Int
alphabet =
    Bech32.Internal.alphabet
        |> String.toList
        |> List.indexedMap (\i c -> ( c, i ))
        |> Dict.fromList


decode : String -> Result DecodeFailure { prefix : String, data : Bytes }
decode rawInput =
    splitFromEnd "1" (String.toLower rawInput)
        |> Result.andThen
            (\{ prefix, payload } ->
                if String.length prefix < 1 then
                    Err <| PrefixTooShort { minimum = 1, currentLength = 0 }

                else if String.length payload < checksumLength then
                    Err <| DataPayloadTooShort { minimum = checksumLength, currentLength = String.length payload }

                else
                    Ok { prefix = prefix, payload = payload }
            )
        |> Result.andThen
            (\{ prefix, payload } ->
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
            )


splitFromEnd : String -> String -> Result DecodeFailure { prefix : String, payload : String }
splitFromEnd sep str =
    let
        last xs =
            case xs of
                [] ->
                    Nothing

                [ i ] ->
                    Just i

                _ :: tail ->
                    last tail
    in
    String.indexes sep str
        |> last
        |> Maybe.map (\i -> Ok { prefix = String.left i str, payload = String.dropLeft (i + 1) str })
        |> Maybe.withDefault (Err MissingSeparator)
