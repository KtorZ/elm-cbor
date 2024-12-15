module Bech32.Encode exposing (encode, EncodeFailure(..))

{-| Encoding arbitrary binary data to Bech32. A Bech32 string consists of:

  - The human-readable part (a.k.a prefix), which is intended to convey the type of data, or anything else that is relevant to the reader. This part MUST contain 1 to 83 US-ASCII characters, with each character having a value in the range [33-126].
  - The separator, which is always "1". Note that "1" is allowed inside the human-readable part and the last one in the string is the separator.
  - The data part, which is at least 6 characters long and only consists of alphanumeric characters excluding "1", "b", "i", and "o".

The last 6 characters of the data-part are an actual checksum of the payload and the prefix, which ensures integrity of the whole string. It also provides bases for an error-detection algorithm, which isn't implemented yet as part of this library.

Importantly, the checksum and the error-detection are most optimal for strings of length smaller than 90 characters. Generating longer strings work, but decreases the efficiency of the checksum in detecting corrupted strings.

@docs encode, EncodeFailure

-}

import Array exposing (Array)
import Bech32.Internal exposing (bytesToWords, checksum, polymodStep)
import Bitwise exposing (and, shiftRightBy)
import Bytes exposing (Bytes)


{-| Failures that might occur when encoding a value. Can be mostly ignored for well-known prefixes.

  - `UnexpectedCharacterInPrefix`: triggered when the prefix contains characters outside of the unicode range [33; 126]. That is, when it contains characters other than:

        !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~

-}
type EncodeFailure
    = UnexpectedCharacterInPrefix { culprit : Char }
    | PrefixTooShort { minimum : Int, currentLength : Int }


{-| Encode a data payload as Bech32 using the given `prefix`. Note that we have the following property:

    Result.andThen Bech32.decode (Bech32.encode { prefix, data }) == Ok({ prefix, data })

-}
encode : { prefix : String, data : Bytes } -> Result EncodeFailure String
encode { prefix, data } =
    if String.length prefix < 1 then
        Err <| PrefixTooShort { minimum = 1, currentLength = 0 }

    else
        checksum (String.toLower prefix)
            |> Result.mapError
                (\c ->
                    UnexpectedCharacterInPrefix { culprit = c }
                )
            |> Result.map
                (\chk0 ->
                    List.foldl
                        (\word ( str, chk ) ->
                            ( String.append str (unsafeCharFrom word)
                            , Bitwise.xor (polymodStep chk) word
                            )
                        )
                        ( String.append prefix "1", chk0 )
                        (bytesToWords data)
                )
            |> Result.map
                (\( str, chk ) ->
                    str ++ suffix chk
                )


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


suffix : Int -> String
suffix chk0 =
    let
        chk : Int
        chk =
            List.foldl (\_ -> polymodStep) chk0 (List.range 0 5) |> Bitwise.xor 1
    in
    List.foldl
        (\k str ->
            let
                i : Int
                i =
                    chk |> shiftRightBy ((5 - k) * 5) |> and 0x1F
            in
            String.append str (unsafeCharFrom i)
        )
        ""
        (List.range 0 5)
