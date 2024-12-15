module Helpers exposing (bytesToWords8, words8ToBytes)

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E


bytesToWords8 : Bytes -> List Int
bytesToWords8 bytes =
    let
        decoder : D.Decoder (List Int)
        decoder =
            D.loop
                ( [], Bytes.width bytes )
                (\( words, width ) ->
                    if width <= 0 then
                        D.succeed <| D.Done <| List.reverse words

                    else
                        D.unsignedInt8 |> D.map (\word -> D.Loop ( word :: words, width - 1 ))
                )
    in
    D.decode decoder bytes |> Maybe.withDefault []


words8ToBytes : List Int -> Bytes
words8ToBytes words =
    words
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
