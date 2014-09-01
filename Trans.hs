module Trans
( transmit
) where

import GHC.Word
import System.Random

transmit :: StdGen -> Word8 -> Word8
transmit gen source = 
    let (rand, newGen) = random gen :: (Float, StdGen)
    in possibly applyInversion rand 0.1 source

possibly :: (Floating f, Ord f) => (a -> a) -> f -> f -> a -> a
possibly f value threshold x = 
    if value < threshold then f x else x

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
