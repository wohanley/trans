module Trans(
  transmit
) where

import GHC.Word

transmit :: Word8 -> Word8
transmit source = applyInversion source

applyInversion :: Word8 -> Word8
applyInversion n = foldl invert n (powersOf2 8)

invert :: Word8 -> Word8 -> Word8
invert n powerOf2 = if n >= powerOf2 then n - powerOf2
                    else n + powerOf2

powersOf2 :: Word8 -> [Word8]
powersOf2 n = map (pow 2) [1..n]

pow :: Word8 -> Word8 -> Word8
pow n 0 = 1
pow n p = n * (pow n (p - 1))
