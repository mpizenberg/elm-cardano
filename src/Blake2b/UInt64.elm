module Blake2b.UInt64 exposing (fromLittleEndianBytes)

import List
import UInt64 exposing (UInt64)


fromLittleEndianBytes : List Int -> UInt64
fromLittleEndianBytes =
    UInt64.fromBigEndianBytes << List.reverse
