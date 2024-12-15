# elm-cardano/bech32

[![](https://img.shields.io/elm-package/v/elm-cardano/bech32.svg?style=for-the-badge)](https://package.elm-lang.org/packages/elm-cardano/bech32/latest/)
[![](https://img.shields.io/github/actions/workflow/status/elm-cardano/bech32/continuous-integration.yml?style=for-the-badge)](https://github.com/elm-cardano/bech32/actions/workflows/continuous-integration.yaml)
[![](https://img.shields.io/github/license/elm-cardano/bech32.svg?style=for-the-badge)](https://github.com/elm-cardano/bech32/blob/main/LICENSE)

A [BIP-0173](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki) implementation of bech32 text encoding and decoding.

## Getting Started

### Installation

```
elm install elm-cardano/bech32
```

### Usage

```elm
-- Generally, one would simply deal with Elm's `Bytes` directly.
words : List Int
words =
    [ 122, 193, 158, 105, 175, 136, 116, 57, 247, 224, 251, 27, 11, 236
    , 48, 252, 141, 208, 43, 15, 115, 180, 209, 244, 26, 54, 207, 244
    ]
```

#### Encoding

```elm
case Bech32.encode { prefix = "pool", data = words8ToBytes words } of
    Ok result ->
        result |> Expect.equal "pool10tqeu6d03p6rnalqlvdshmpsljxaq2c0ww6draq6xm8lgyl2a3p"

    Err failure ->
        Debug.toString failure |> Expect.fail
```

#### Decoding

```elm
case Bech32.decode "pool10tqeu6d03p6rnalqlvdshmpsljxaq2c0ww6draq6xm8lgyl2a3p" of
    Ok result ->
        Expect.all
            [ \subject -> subject.prefix |> Expect.equal "pool"
            , \subject -> bytesToWords8 subject.data |> Expect.equal words
            ]
            result

    Err failure ->
        Debug.toString failure |> Expect.fail
```

## Changelog

See [CHANGELOG.md](https://github.com/elm-cardano/bech32/tree/main/CHANGELOG.md)
