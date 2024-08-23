module Blake2b exposing (blake2bIV, mixing, sigmaRound)

import Bitwise
import Blake2b.Int64 as Int64 exposing (Int64(..))
import Extra.List as List



-- Compression function takes the state vector ("h" in spec),
-- message block vector ("m" in spec),
-- a 2w-bit (128-bit) offset counter ("t" in spec),
-- and final block indicator flag ("f" in spec),
-- and returns the new state vector.


compress : List Int64 -> List Int -> { low : Int64, high : Int64 } -> Bool -> Maybe (List Int64)
compress state block ctr finalBlockFlag =
    let
        -- Pad the block and convert to U64 words
        -- blockWords =
        mBlockWords : Maybe (List Int64)
        mBlockWords =
            (block ++ List.repeat (128 - List.length block) 0x00)
                |> List.chunksOf 8
                |> List.map Int64.fromLeByteValues
                |> List.foldr
                    (\mByte mAcc ->
                        case ( mByte, mAcc ) of
                            ( Just b, Just acc ) ->
                                Just (b :: acc)

                            _ ->
                                Nothing
                    )
                    (Just [])

        -- Initialize local vector with state and IV
        vInit : List Int64
        vInit =
            (state ++ blake2bIV)
                |> List.indexedMap
                    (\i word ->
                        if i == 12 then
                            Int64.or word ctr.low

                        else if i == 13 then
                            Int64.or word ctr.high

                        else if i == 14 then
                            if finalBlockFlag then
                                Int64.or word Int64.maxValue

                            else
                                word

                        else
                            word
                    )

        -- Cryptographic mixing step
        mSigmaMixingStep : Quadruple64 -> Int64 -> Int64 -> List Int64 -> Maybe (List Int64)
        mSigmaMixingStep { a, b, c, d } si sj v =
            mBlockWords
                |> Maybe.andThen
                    (\blockWords ->
                        let
                            x =
                                blockWords |> List.get64 si |> Maybe.withDefault (Int64 0 0)

                            y =
                                blockWords |> List.get64 sj |> Maybe.withDefault (Int64 0 0)

                            va =
                                List.get64 a v |> Maybe.withDefault (Int64 0 0)

                            vb =
                                List.get64 b v |> Maybe.withDefault (Int64 0 0)

                            vc =
                                List.get64 c v |> Maybe.withDefault (Int64 0 0)

                            vd =
                                List.get64 d v |> Maybe.withDefault (Int64 0 0)

                            -- { vaNew, vbNew, vcNew, vdNew } =
                            newVsQuad =
                                mixing va vb vc vd x y
                        in
                        v
                            |> List.indexedMap64
                                (\i v0 ->
                                    if i == a then
                                        newVsQuad.a

                                    else if i == b then
                                        newVsQuad.b

                                    else if i == c then
                                        newVsQuad.c

                                    else if i == d then
                                        newVsQuad.d

                                    else
                                        v0
                                )
                            |> Just
                    )

        -- Cryptographic mixing round
        mSigmaMixingRound : Int -> List Int64 -> Maybe (List Int64)
        mSigmaMixingRound round v =
            let
                s =
                    sigmaRound round
            in
            mSigmaMixingStep (q64FromInts 0 4 8 12) s.i00 s.i01 v
                |> Maybe.andThen (mSigmaMixingStep (q64FromInts 1 5 9 13) s.i02 s.i03)
                |> Maybe.andThen (mSigmaMixingStep (q64FromInts 2 6 10 14) s.i04 s.i05)
                |> Maybe.andThen (mSigmaMixingStep (q64FromInts 3 7 11 15) s.i06 s.i07)
                |> Maybe.andThen (mSigmaMixingStep (q64FromInts 0 5 10 15) s.i08 s.i09)
                |> Maybe.andThen (mSigmaMixingStep (q64FromInts 1 6 11 12) s.i10 s.i11)
                |> Maybe.andThen (mSigmaMixingStep (q64FromInts 2 7 8 13) s.i12 s.i13)
                |> Maybe.andThen (mSigmaMixingStep (q64FromInts 3 4 9 14) s.i14 s.i15)

        -- Apply 12 mixing rounds to the local vector
        mNewV =
            mSigmaMixingRound 0 vInit
                |> Maybe.andThen (mSigmaMixingRound 1)
                |> Maybe.andThen (mSigmaMixingRound 2)
                |> Maybe.andThen (mSigmaMixingRound 3)
                |> Maybe.andThen (mSigmaMixingRound 4)
                |> Maybe.andThen (mSigmaMixingRound 5)
                |> Maybe.andThen (mSigmaMixingRound 6)
                |> Maybe.andThen (mSigmaMixingRound 7)
                |> Maybe.andThen (mSigmaMixingRound 8)
                |> Maybe.andThen (mSigmaMixingRound 9)
                |> Maybe.andThen (mSigmaMixingRound 10)
                |> Maybe.andThen (mSigmaMixingRound 11)
    in
    mNewV
        |> Maybe.andThen
            (\newV ->
                -- XOR the two halves of v
                List.map3
                    (\hi vi vii ->
                        Int64.xor hi vi |> Int64.xor vii
                    )
                    state
                    (List.sublist 0 8 newV)
                    (List.sublist 8 8 newV)
                    |> Just
            )


blake2bIV : List Int64
blake2bIV =
    [ Int64 0x6A09E667 0xF3BCC908
    , Int64 0xBB67AE85 0x84CAA73B
    , Int64 0x3C6EF372 0xFE94F82B
    , Int64 0xA54FF53A 0x5F1D36F1
    , Int64 0x510E527F 0xADE682D1
    , Int64 0x9B05688C 0x2B3E6C1F
    , Int64 0x1F83D9AB 0xFB41BD6B
    , Int64 0x5BE0CD19 0x137E2179
    ]



-- The mixing function mixes two words x, y with four
-- words v[a], v[b], v[c], v[d] and returns these four words modified. The
-- vector v is the working vector of the blake algorithm.
--
-- mixing va, vb, vc, vd, x, y -> new (va, vb, vc, vd)


type alias Quadruple64 =
    { a : Int64
    , b : Int64
    , c : Int64
    , d : Int64
    }


q64FromInts : Int -> Int -> Int -> Int -> Quadruple64
q64FromInts a b c d =
    let
        int64 =
            Int64 0
    in
    Quadruple64 (int64 a) (int64 b) (int64 c) (int64 d)


mixing : Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Quadruple64
mixing va vb vc vd x y =
    let
        vaTemp =
            Int64.add va vb |> Int64.add x

        -- R1 = 32
        vdTemp =
            Int64.xor vd vaTemp |> Int64.rotateRightBy 32

        vcTemp =
            Int64.add vc vdTemp

        -- R2 = 24
        vbTemp =
            Int64.xor vb vcTemp |> Int64.rotateRightBy 24

        vaNew =
            Int64.add vaTemp vbTemp |> Int64.add y

        -- R3 = 16
        vdNew =
            Int64.xor vdTemp vaNew |> Int64.rotateRightBy 16

        vcNew =
            Int64.add vcTemp vdNew

        -- R4 = 63
        vbNew =
            Int64.xor vbTemp vcNew |> Int64.rotateRightBy 63
    in
    Quadruple64 vaNew vbNew vcNew vdNew


type alias SigmaRound =
    { i00 : Int64
    , i01 : Int64
    , i02 : Int64
    , i03 : Int64
    , i04 : Int64
    , i05 : Int64
    , i06 : Int64
    , i07 : Int64
    , i08 : Int64
    , i09 : Int64
    , i10 : Int64
    , i11 : Int64
    , i12 : Int64
    , i13 : Int64
    , i14 : Int64
    , i15 : Int64
    }


intsToSigmaRound : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> SigmaRound
intsToSigmaRound i00 i01 i02 i03 i04 i05 i06 i07 i08 i09 i10 i11 i12 i13 i14 i15 =
    let
        int64 =
            Int64 0
    in
    SigmaRound
        (int64 i00)
        (int64 i01)
        (int64 i02)
        (int64 i03)
        (int64 i04)
        (int64 i05)
        (int64 i06)
        (int64 i07)
        (int64 i08)
        (int64 i09)
        (int64 i10)
        (int64 i11)
        (int64 i12)
        (int64 i13)
        (int64 i14)
        (int64 i15)



-- Selection permutation for one mixing round


sigmaRound : Int -> SigmaRound
sigmaRound round =
    case round |> Bitwise.shiftRightZfBy 0 of
        0 ->
            intsToSigmaRound 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

        1 ->
            intsToSigmaRound 14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3

        2 ->
            intsToSigmaRound 11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4

        3 ->
            intsToSigmaRound 7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8

        4 ->
            intsToSigmaRound 9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13

        5 ->
            intsToSigmaRound 2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9

        6 ->
            intsToSigmaRound 12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11

        7 ->
            intsToSigmaRound 13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10

        8 ->
            intsToSigmaRound 6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5

        9 ->
            intsToSigmaRound 10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0

        _ ->
            sigmaRound (modBy 10 round)
