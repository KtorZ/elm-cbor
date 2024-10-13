module Bech32.DecodeTests exposing (suite)

import Bech32.Decode as Bech32
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bech32.Internal"
        [ describe "decode" <|
            let
                testDecodeOk : String -> String -> List Int -> Test
                testDecodeOk str prefix words =
                    test ("decode " ++ str) <|
                        \_ ->
                            case Bech32.decode str of
                                Ok result ->
                                    Expect.all
                                        [ \subject -> subject.prefix |> Expect.equal prefix
                                        , \subject -> subject.words |> Expect.equal words
                                        ]
                                        result

                                Err failure ->
                                    Expect.fail <| Debug.toString failure
            in
            [ testDecodeOk
                "addr1q83ejwrp482r6md2vfdwhx5fu5cytx7tcvmg6gsu68j8hp960j0580reh0c9nntvc2mjx5ryzje2rvw0l7j23c3gr6cshr82sa"
                "addr"
                [ 1, 227, 153, 56, 97, 169, 212, 61, 109, 170, 98, 90, 235, 154, 137, 229, 48, 69, 155, 203, 195, 54, 141, 34, 28, 209, 228, 123, 132, 186, 124, 159, 67, 188, 121, 187, 240, 89, 205, 108, 194, 183, 35, 80, 100, 20, 178, 161, 177, 207, 255, 164, 168, 226, 40, 30, 177 ]
            , testDecodeOk
                "stake1uxjnd0vjyhpc0cvn20prx8ztn22dusacyuhhsrk4t9k9uksnhzpqa"
                "stake"
                [ 225, 165, 54, 189, 146, 37, 195, 135, 225, 147, 83, 194, 51, 28, 75, 154, 148, 222, 67, 184, 39, 47, 120, 14, 213, 89, 108, 94, 90 ]
            , testDecodeOk
                "drep1y2yph8yyln805s9em8gr0kqxxh4xuh73jpg24dclauhp9ucu0tkaj"
                "drep"
                [ 34, 136, 27, 156, 132, 252, 206, 250, 64, 185, 217, 208, 55, 216, 6, 53, 234, 110, 95, 209, 144, 80, 170, 183, 31, 239, 46, 18, 243 ]
            , testDecodeOk
                "pool10tqeu6d03p6rnalqlvdshmpsljxaq2c0ww6draq6xm8lgyl2a3p"
                "pool"
                [ 122, 193, 158, 105, 175, 136, 116, 57, 247, 224, 251, 27, 11, 236, 48, 252, 141, 208, 43, 15, 115, 180, 209, 244, 26, 54, 207, 244 ]
            ]
        ]
