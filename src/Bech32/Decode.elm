module Bech32.Decode exposing (decode, DecodeFailure(..), WordsToBytesFailure(..))

{-| Decoding Bech32 strings to binary data. A Bech32 string consists of:

  - The human-readable part (a.k.a prefix), which is intended to convey the type of data, or anything else that is relevant to the reader. This part MUST contain 1 to 83 US-ASCII characters, with each character having a value in the range [33-126].
  - The separator, which is always "1". Note that "1" is allowed inside the human-readable part and the last one in the string is the separator.
  - The data part, which is at least 6 characters long and only consists of alphanumeric characters excluding "1", "b", "i", and "o".

The last 6 characters of the data-part are an actual checksum of the payload and the prefix, which ensures integrity of the whole string. It also provides bases for an error-detection algorithm, which isn't implemented yet as part of this library.

Importantly, the checksum and the error-detection are most optimal for strings of length smaller than 90 characters. Generating longer strings work, but decreases the efficiency of the checksum in detecting corrupted strings.

@docs decode, DecodeFailure, WordsToBytesFailure

-}

import Bech32.Internal exposing (checksum, checksumLength, polymodStep, wordsToBytes)
import Bitwise
import Bytes exposing (Bytes)
import Dict exposing (Dict)


{-| Failures that might occur when decoding a value.
-}
type DecodeFailure
    = DataPayloadTooShort { minimum : Int, currentLength : Int }
    | PrefixTooShort { minimum : Int, currentLength : Int }
    | UnexpectedCharacterInPayload { culprit : Char }
    | UnexpectedCharacterInPrefix { culprit : Char }
    | InvalidChecksum
    | MissingSeparator
    | InternalError WordsToBytesFailure


{-| Failure to decode words into bytes; should never occur in practice because Elm's `Bytes` are well-formed by construction
-}
type WordsToBytesFailure
    = ExcessPadding
    | NonZeroPadding


{-| Decode a Bech32 string into its internal constituents. Note that we have the following property:

    Result.andThen Bech32.decode (Bech32.encode { prefix, data }) == Ok({ prefix, data })

-}
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
                                                        isChecksum : Bool
                                                        isChecksum =
                                                            cursor <= 0

                                                        newChk : Int
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
                                    |> wordsToBytes ExcessPadding NonZeroPadding
                                    |> Result.mapError InternalError
                        )
                    |> Result.map
                        (\data ->
                            { prefix = prefix
                            , data = data
                            }
                        )
            )


alphabet : Dict Char Int
alphabet =
    Bech32.Internal.alphabet
        |> String.toList
        |> List.indexedMap (\i c -> ( c, i ))
        |> Dict.fromList


splitFromEnd : String -> String -> Result DecodeFailure { prefix : String, payload : String }
splitFromEnd sep str =
    let
        last : List a -> Maybe a
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
