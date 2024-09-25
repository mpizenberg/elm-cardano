module Cardano.HardwareWallet exposing (suite)

import Bytes.Comparable as Bytes
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), NetworkId(..))
import Cardano.AuxiliaryData exposing (AuxiliaryData)
import Cardano.Data as Data
import Cardano.Gov exposing (Action(..), Drep(..), Vote(..), Voter(..), noParamUpdate)
import Cardano.Metadatum as Metadatum
import Cardano.Transaction as Transaction exposing (Certificate(..), TransactionBody, WitnessSet, newBody, newWitnessSet)
import Cardano.Utxo as Utxo exposing (DatumOption(..))
import Cardano.Value as Value
import Cbor.Decode as D
import Dict exposing (Dict)
import Expect
import Integer
import Natural as N exposing (Natural)
import Test exposing (Test, describe, test)


suite : Test
suite =
    -- Verification of validity can be done with cardano-hw-cli
    -- https://github.com/vacuumlabs/cardano-hw-cli
    describe "Transaction encoding must be Hardware Wallet (HW) compliant (CIP-21)"
        [ test "Simple transaction" <|
            \_ ->
                let
                    expectedEncoding =
                        "84a40081825820bc8bf52ea894fb8e442fe3eea628be87d0c9a37baef185b70eb00a5c8a849d3b000181a20058390180f9e2c88e6c817008f3a812ed889b4a4da8e0bd103f86e7335422aa122a946b9ad3d2ddf029d3a828f0468aece76895f15c9efbd69b4277011a0023583c021a00029b75031a01a3bd8fa0f5f6"

                    tx =
                        { auxiliaryData = Nothing
                        , body =
                            { newBody
                                | auxiliaryDataHash = Nothing
                                , fee = Just (N.fromSafeInt 170869)
                                , inputs = [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "bc8bf52ea894fb8e442fe3eea628be87d0c9a37baef185b70eb00a5c8a849d3b" } ]
                                , outputs =
                                    [ { address =
                                            Address.base Mainnet
                                                (VKeyHash (Bytes.fromStringUnchecked "80f9e2c88e6c817008f3a812ed889b4a4da8e0bd103f86e7335422aa"))
                                                (VKeyHash (Bytes.fromStringUnchecked "122a946b9ad3d2ddf029d3a828f0468aece76895f15c9efbd69b4277"))
                                      , amount = Value.onlyLovelace (N.fromSafeInt 2316348)
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    ]
                                , ttl = Just (N.fromSafeInt 27508111)
                            }
                        , isValid = True
                        , witnessSet = newWitnessSet
                        }
                in
                Transaction.serialize tx
                    |> Expect.equal (Bytes.fromStringUnchecked expectedEncoding)
        ]



-- Helpers


{-| Convert the internal representation of Natural, using a base 2^26, back into a Natural.
-}
bigNat : List Int -> Natural
bigNat xs =
    let
        step x ( n, base ) =
            ( N.add n (N.mul base (N.fromSafeInt x))
              -- base * 2**26
            , N.mul base (N.fromSafeInt 0x04000000)
            )
    in
    List.foldl step ( N.zero, N.one ) xs
        |> Tuple.first


bytesMap : Dict String v -> BytesMap k v
bytesMap keyValues =
    Dict.toList keyValues
        |> List.map (\( k, v ) -> ( Bytes.fromStringUnchecked k, v ))
        |> Bytes.Map.fromList
