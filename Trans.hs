module Trans
( transmit
) where

import GHC.Word
import System.Random

errors :: (Floating f) => [((Word8 -> Word8), f)]
errors =    [ (applyInversion, 0.01)
            , (applyFlipping, 0.1)
            ]

applyError :: (Word8, StdGen) -> ((Word8 -> Word8), Float) -> (Word8, StdGen)
applyError (acc, gen) (error, probability) = 
    let (rand, gen') = random gen :: (Float, StdGen)
    in (possibly error rand probability acc, gen')

transmit :: StdGen -> Word8 -> Word8
transmit gen source = 
    let (result, _) = foldl applyError (source, gen) errors
    in result

possibly :: (Floating f, Ord f) => (a -> a) -> f -> f -> a -> a
possibly f value threshold x = 
    if value < threshold then f x else x

applyInversion :: Word8 -> Word8
applyInversion n = foldl invert n (powersOf2 8)

invert :: Word8 -> Word8 -> Word8
invert n powerOf2 = if n >= powerOf2 then n - powerOf2
                    else n + powerOf2

applyFlipping :: Word8 -> Word8
applyFlipping n = foldl (Trans.flip (powersOf2 8)) n [0..5]

flip :: [Word8] -> Word8 -> Int -> Word8
flip powersOf2 n powerIndex =
    invertAt (invertAt n powersOf2 powerIndex) powersOf2 (powerIndex + 2)

invertAt :: Word8 -> [Word8] -> Int -> Word8
invertAt n powersOf2 powerIndex = invert n (powersOf2 !! powerIndex)

powersOf2 :: Word8 -> [Word8]
powersOf2 n = map (pow 2) [1..n]

pow :: Word8 -> Word8 -> Word8
pow n 0 = 1
pow n p = n * (pow n (p - 1))
