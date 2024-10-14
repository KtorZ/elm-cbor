module Bech32.InternalTests exposing (suite)

import Bech32.Internal exposing (WordsToBytesFailure(..), bytesToWords, checksum, polymodStep, wordsToBytes)
import Expect
import Helpers exposing (bytesToWords8, words8ToBytes)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bech32.Internal"
        [ describe "polymodStep" <|
            let
                testPolymod : Int -> Int -> Test
                testPolymod pre result =
                    test ("polymodStep " ++ String.fromInt pre ++ " == " ++ String.fromInt result) <|
                        \_ ->
                            polymodStep pre |> Expect.equal result
            in
            [ testPolymod 1 32
            , testPolymod 35 1120
            , testPolymod 1123 35936
            , testPolymod 35939 1150048
            , testPolymod 1150051 36801632
            , testPolymod 36801635 1029430226
            , testPolymod 1029430226 81575097
            , testPolymod 81575082 1036458797
            , testPolymod 1036458809 390373849
            , testPolymod 390373848 149737730
            , testPolymod 149737737 54212826
            , testPolymod 54212831 470633554
            , testPolymod 470633550 68824458
            , testPolymod 68824460 622149613
            , testPolymod 622149631 786633534
            , testPolymod 786633517 904775478
            , testPolymod 904775483 213792867
            , testPolymod 213792876 792107543
            , testPolymod 792107547 257745398
            , testPolymod 257745380 671612069
            , testPolymod 671612065 877326185
            , testPolymod 877326206 945500355
            , testPolymod 945500354 43077844
            , testPolymod 43077836 692080178
            , testPolymod 692080189 482131177
            , testPolymod 482131185 304364138
            , testPolymod 304364134 45033647
            , testPolymod 45033660 781140018
            , testPolymod 781140024 946316694
            , testPolymod 946316697 84147124
            , testPolymod 84147125 103807181
            , testPolymod 103807182 415612959
            , testPolymod 415612953 995830023
            , testPolymod 995830016 435699494
            , testPolymod 435699492 484757159
            , testPolymod 484757164 422212042
            , testPolymod 422212057 112005383
            , testPolymod 112005389 144242815
            , testPolymod 144242805 230561626
            , testPolymod 230561623 255032695
            , testPolymod 255032683 636775749
            , testPolymod 636775765 852051582
            , testPolymod 852051555 890555068
            , testPolymod 890555044 329223043
            , testPolymod 329223047 903419023
            , testPolymod 903419027 170151267
            , testPolymod 170151284 558031048
            , testPolymod 558031071 38304083
            , testPolymod 38304067 845512658
            , testPolymod 845512657 546569468
            , testPolymod 546569450 946601971
            , testPolymod 946601958 76067924
            , testPolymod 76067935 927335821
            , testPolymod 927335816 581712305
            , testPolymod 581712295 83427809
            , testPolymod 83427812 963687149
            , testPolymod 963687153 669062836
            , testPolymod 669062818 185364268
            , testPolymod 185364284 71739848
            , testPolymod 71739867 780514573
            , testPolymod 780514586 960037334
            , testPolymod 960037332 785871380
            , testPolymod 785871381 862052406
            , testPolymod 862052406 28
            ]
        , describe "checksum" <|
            let
                testChecksum : String -> Result Char Int -> Test
                testChecksum prefix chk =
                    test ("checksum " ++ prefix ++ " == " ++ Result.withDefault "Err" (Result.map String.fromInt chk)) <|
                        \_ ->
                            checksum prefix |> Expect.equal chk
            in
            [ testChecksum "stake" (Ok 54212831)
            , testChecksum "addr" (Ok 393510859)
            , testChecksum "pool" (Ok 394056373)
            , testChecksum "drep" (Ok 393652201)
            , testChecksum "!" (Ok 33793)
            , testChecksum "~" (Ok 35870)
            , testChecksum " " (Err ' ')
            , testChecksum "\u{007F}" (Err '\u{007F}')
            ]
        , describe "encodeWords" <|
            let
                testEncodeWordsOk : List Int -> List Int -> Test
                testEncodeWordsOk words5 words8 =
                    describe ("encodeWords\n\twords 5: " ++ Debug.toString words5 ++ "\n\twords 8: " ++ Debug.toString words8) <|
                        [ test "5 → 8" <|
                            \_ -> wordsToBytes words5 |> Result.map bytesToWords8 |> Expect.equal (Ok words8)
                        , test "8 → 5" <|
                            \_ -> bytesToWords (words8ToBytes words8) |> Expect.equal words5
                        ]
            in
            [ testEncodeWordsOk
                [ 28, 6, 18, 19, 13, 15, 12, 18, 4, 23, 1, 24, 15, 24, 12, 19, 10, 15, 1, 3, 6, 7, 2, 11, 19, 10, 10, 13, 28, 16, 29, 24, 4, 28, 23, 23, 16, 3, 22, 21, 11, 5, 22, 5, 28, 22, 16 ]
                [ 225, 165, 54, 189, 146, 37, 195, 135, 225, 147, 83, 194, 51, 28, 75, 154, 148, 222, 67, 184, 39, 47, 120, 14, 213, 89, 108, 94, 90 ]
            , testEncodeWordsOk
                [ 28, 7, 2, 17, 23, 14, 25, 14, 28, 4, 23, 1, 28, 5, 12, 4, 28, 23, 23, 26, 19, 18, 9, 4, 10, 12, 14, 23, 1, 29, 8, 26, 2, 10, 11, 9, 21, 6, 19, 5, 3, 28, 29, 25, 5, 29, 24 ]
                [ 225, 197, 27, 187, 46, 225, 46, 30, 21, 132, 229, 239, 169, 201, 36, 83, 29, 112, 245, 26, 18, 150, 154, 154, 101, 31, 59, 146, 247 ]
            , testEncodeWordsOk
                [ 4, 10, 4, 1, 23, 7, 4, 4, 31, 19, 7, 15, 20, 16, 5, 25, 27, 7, 8, 3, 15, 22, 0, 6, 6, 23, 21, 6, 28, 23, 30, 17, 18, 1, 8, 10, 21, 13, 24, 31, 29, 28, 23, 1, 5, 28, 24 ]
                [ 34, 136, 27, 156, 132, 252, 206, 250, 64, 185, 217, 208, 55, 216, 6, 53, 234, 110, 95, 209, 144, 80, 170, 183, 31, 239, 46, 18, 243 ]
            , testEncodeWordsOk
                [ 4, 11, 17, 30, 16, 17, 2, 17, 13, 13, 2, 27, 2, 28, 8, 5, 27, 28, 18, 6, 4, 20, 24, 15, 1, 29, 11, 8, 25, 11, 27, 18, 12, 26, 9, 23, 9, 17, 23, 25, 21, 19, 27, 19, 12, 10, 8 ]
                [ 34, 227, 232, 68, 81, 107, 69, 177, 113, 5, 223, 36, 98, 83, 15, 15, 86, 140, 175, 114, 102, 147, 116, 198, 249, 172, 247, 54, 41 ]
            , test "encodeWords -> excess padding" <|
                \_ -> wordsToBytes [ 14, 20, 15, 7, 13, 26, 0, 25, 18, 6, 11, 13, 8, 21, 4, 20, 3, 17, 2, 29, 3, 0 ] |> Expect.equal (Err ExcessPadding)
            , test "encodeWords -> non-zero padding" <|
                \_ -> wordsToBytes [ 3, 1, 17, 17, 8, 15, 0, 20, 24, 20, 11, 6, 16, 1, 5, 29, 3, 4, 16, 3, 6, 21, 22, 26, 2, 13, 22, 9, 16, 21, 19, 24, 25, 21, 6, 18, 15, 8, 13, 24, 24, 24, 25, 9, 12, 1, 4, 16, 6, 9, 17, 1 ] |> Expect.equal (Err NonZeroPadding)
            ]
        ]
