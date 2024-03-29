module Trans
( transmit
) where

import GHC.Word
import System.Random

errors :: [(StdGen, Word8) -> (StdGen, Word8)]
errors =    [ applyInversion 0.01
            , applyFlipping 0.01
            ]

transmit :: StdGen -> Word8 -> Word8
transmit gen source = 
    let (_, result) = foldl applyError (gen, source) errors
    in result

applyError :: (StdGen, Word8) -> ((StdGen, Word8) -> (StdGen, Word8))
    -> (StdGen, Word8)
applyError (gen, acc) (error) = error (gen, acc)

possibly :: (Ord o) => (a -> a) -> o -> o -> (a -> a)
possibly f value threshold = 
    if value < threshold then f else id

applyInversion :: Float -> (StdGen, Word8) -> (StdGen, Word8)
applyInversion probability (gen, n) =
    foldl (inversionFoldFunction probability) (gen, n) (powersOf2 8)

-- I want this in a where clause but I can't get the syntax to work
inversionFoldFunction probability (gen, acc) powerOf2 =
    let (rand, gen') = random gen :: (Float, StdGen)
    in (gen', possibly (invert powerOf2) rand probability acc)

invert :: Word8 -> Word8 -> Word8
invert powerOf2 n = if n >= powerOf2 then n - powerOf2
                    else n + powerOf2

applyFlipping :: Float -> (StdGen, Word8) -> (StdGen, Word8)
applyFlipping probability (gen, n) =
    foldl (flipFoldFunction probability) (gen, n) [0..5]

flipFoldFunction probability (gen, acc) powerOf2 =
    let (rand, gen') = random gen :: (Float, StdGen)
    in (gen', possibly (Trans.flip (powersOf2 8) powerOf2) rand probability acc)

flip :: [Word8] -> Int -> Word8 -> Word8
flip powersOf2 powerIndex n =
    invertAt (invertAt n powersOf2 powerIndex) powersOf2 (powerIndex + 2)

invertAt :: Word8 -> [Word8] -> Int -> Word8
invertAt n powersOf2 powerIndex = invert (powersOf2 !! powerIndex) n

powersOf2 :: Word8 -> [Word8]
powersOf2 n = [ pow 2 x | x <- [1..n] ]

pow :: Word8 -> Word8 -> Word8
pow n 0 = 1
pow n p = n * (pow n (p - 1))
