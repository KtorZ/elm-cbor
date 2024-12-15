# Changelog

## v1.0.0 - 2024-12-15

### Added

#### Bech32.Encode

- ```elm
  {-| Encode a data payload as Bech32 using the given `prefix`. -}
  encode : { prefix : String, data : Bytes } -> Result EncodeFailure String
  ```

- ```elm
  type EncodeFailure
      = UnexpectedCharacterInPrefix { culprit : Char }
      | PrefixTooShort { minimum : Int, currentLength : Int }
  ```

#### Bech32.Decode

- ```elm
  {-| Decode a Bech32 string into its internal constituents. -}
  decode : String -> Result DecodeFailure { prefix : String, data : Bytes }
  ```

- ```elm
  type DecodeFailure
      = DataPayloadTooShort { minimum : Int, currentLength : Int }
      | PrefixTooShort { minimum : Int, currentLength : Int }
      | UnexpectedCharacterInPayload { culprit : Char }
      | UnexpectedCharacterInPrefix { culprit : Char }
      | InvalidChecksum
      | MissingSeparator
      | InternalError WordsToBytesFailure
  ```

- ```elm
  type WordsToBytesFailure
      = ExcessPadding
      | NonZeroPadding
  ```

### Changed

N/A

### Removed

N/A
