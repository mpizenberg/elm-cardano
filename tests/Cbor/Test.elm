module Cbor.Test exposing (..)

import Cbor.Decode as D
import Cbor.Encode as E
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, fuzz)


roundtrip : (a -> E.Encoder) -> D.Decoder a -> Fuzzer a -> Test
roundtrip encode decode fuzzer =
    fuzz fuzzer "rountrip: Cbor.encode >> Cbor.decode" <|
        \a ->
            D.decode decode (E.encode (encode a))
                |> Expect.equal (Just a)
