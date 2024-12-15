module Bech32Tests exposing (suite)

import Bech32.Decode as Bech32
import Bech32.Encode as Bech32
import Expect
import Fuzz exposing (Fuzzer)
import Helpers exposing (bytesToWords8, words8ToBytes)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "encode >> decode"
        [ fuzz bech32Components "bech32 components" <|
            \st ->
                Bech32.encode { prefix = st.prefix, data = words8ToBytes st.words }
                    |> Result.mapError Debug.toString
                    |> Result.andThen (Bech32.decode >> Result.mapError Debug.toString)
                    |> Result.map (\r -> { prefix = r.prefix, words = bytesToWords8 r.data })
                    |> Expect.equal (Ok { st | prefix = String.toLower st.prefix })
        ]


bech32Components : Fuzzer { prefix : String, words : List Int }
bech32Components =
    let
        prefixChar : Fuzzer Char
        prefixChar =
            Fuzz.intRange 33 126 |> Fuzz.map Char.fromCode
    in
    Fuzz.listOfLengthBetween 1 10 prefixChar
        |> Fuzz.andThen
            (\prefix ->
                Fuzz.list (Fuzz.intRange 0 255)
                    |> Fuzz.map
                        (\words ->
                            { prefix = String.fromList prefix, words = words }
                        )
            )
