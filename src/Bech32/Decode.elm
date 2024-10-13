module Bech32.Decode exposing (Bech32Object, DecoderFailure(..), decode)

import Bech32.Internal exposing (EncodingDirection(..), WordEncodingFailure, alphabet, checksum, checksumLength, encodeWords, polymodStep)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Dict


type alias Bech32Object =
    { prefix : String
    , words : List Int
    , asBytes : () -> Bytes
    }


type DecoderFailure
    = DataPayloadTooShort { minimum : Int, currentLength : Int }
    | UnexpectedCharacterInPayload { culprit : Char }
    | UnexpectedCharacterInPrefix { culprit : Char }
    | InvalidChecksum
    | MissingSeparator
    | EmptyDataPayload
    | TooManySeparators
    | InternalError WordEncodingFailure


decode : String -> Result DecoderFailure Bech32Object
decode rawInput =
    case String.split "1" (String.toLower rawInput) of
        [] ->
            Err MissingSeparator

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
                                    |> encodeWords Word5ToWord8
                                    |> Result.mapError InternalError
                        )
                    |> Result.map
                        (\words ->
                            { prefix = prefix
                            , words = words
                            , asBytes =
                                \() ->
                                    words
                                        |> List.map BE.unsignedInt8
                                        |> BE.sequence
                                        |> BE.encode
                            }
                        )

        _ ->
            Err TooManySeparators
