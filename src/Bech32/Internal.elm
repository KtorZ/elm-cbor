module Bech32.Internal exposing (..)

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E


checksumLength : Int
checksumLength =
    6


alphabet : String
alphabet =
    "qpzry9x8gf2tvdw0s3jn54khce6mua7l"


checksum : String -> Result Char Int
checksum prefix =
    prefix
        |> String.foldl
            (\c ->
                Result.andThen <|
                    \chk ->
                        let
                            i : Int
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
        b : Int
        b =
            pre |> shiftRightBy 25

        step : Int -> Int -> Int -> Int
        step n magicNumber =
            Bitwise.xor (and -(and (shiftRightBy n b) 1) magicNumber)
    in
    shiftLeftBy 5 (and pre 0x01FFFFFF)
        |> step 0 0x3B6A57B2
        |> step 1 0x26508E6D
        |> step 2 0x1EA119FA
        |> step 3 0x3D4233DD
        |> step 4 0x2A1462B3


wordSize : Int
wordSize =
    5


maxWord : Int
maxWord =
    31


byteSize : Int
byteSize =
    8


maxByte : Int
maxByte =
    255


wordsToBytes : err -> err -> List Int -> Result err Bytes
wordsToBytes excessPadding nonZeroPadding words =
    let
        ( finalValue, finalBits, finalBytes ) =
            List.foldl
                (\word ( value0, bits0, bytes0 ) ->
                    let
                        value : Int
                        value =
                            value0
                                |> shiftLeftBy wordSize
                                |> or word

                        ( bytes, bits ) =
                            nextBytes value bytes0 (bits0 + wordSize)
                    in
                    ( value, bits, bytes )
                )
                ( 0, 0, E.sequence [] )
                words
    in
    if finalBits >= wordSize then
        Err excessPadding

    else
        let
            padding : Int
            padding =
                finalValue
                    |> shiftLeftBy (byteSize - finalBits)
                    |> and maxByte
        in
        if padding /= 0 then
            Err nonZeroPadding

        else
            Ok (E.encode finalBytes)


bytesToWords : Bytes -> List Int
bytesToWords bytes =
    let
        decoder : D.Decoder (List Int)
        decoder =
            D.loop
                { value = 0, bits = 0, words = [], width = Bytes.width bytes }
                (\st ->
                    if st.width <= 0 then
                        (if st.bits > 0 then
                            let
                                padding : Int
                                padding =
                                    st.value |> shiftLeftBy (wordSize - st.bits) |> and maxWord
                            in
                            padding :: st.words

                         else
                            st.words
                        )
                            |> List.reverse
                            |> D.Done
                            |> D.succeed

                    else
                        D.unsignedInt8
                            |> D.map
                                (\byte ->
                                    let
                                        value : Int
                                        value =
                                            st.value
                                                |> shiftLeftBy byteSize
                                                |> or byte
                                    in
                                    D.Loop <|
                                        nextWords
                                            { st
                                                | value = value
                                                , width = st.width - 1
                                                , bits = st.bits + byteSize
                                            }
                                )
                )
    in
    -- NOTE: The 'Nothing' case is impossible here as bytes always form
    -- a valid list of word5. So we unwrap the decoder error as an empty
    -- list to avoid polluting the callee down the line.
    D.decode decoder bytes |> Maybe.withDefault []


nextWords : { r | value : Int, bits : Int, words : List Int, width : Int } -> { r | value : Int, bits : Int, words : List Int, width : Int }
nextWords st =
    if st.bits >= wordSize then
        let
            bits : Int
            bits =
                st.bits - wordSize

            word : Int
            word =
                st.value |> shiftRightBy bits |> and maxWord
        in
        nextWords { st | bits = bits, words = word :: st.words }

    else
        st


nextBytes : Int -> E.Encoder -> Int -> ( E.Encoder, Int )
nextBytes value encoder bits0 =
    if bits0 >= byteSize then
        let
            bits : Int
            bits =
                bits0 - byteSize

            word : Int
            word =
                value |> shiftRightBy bits |> and maxByte
        in
        nextBytes value (E.sequence [ encoder, E.unsignedInt8 word ]) bits

    else
        ( encoder, bits0 )
