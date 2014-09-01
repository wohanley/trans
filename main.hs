import Data.ByteString.Lazy as BS
import Trans
import System.Random
import GHC.Word

main = do
    stream <- BS.getContents
    loadedTransmit <- genToTransmit
    BS.putStr . BS.map loadedTransmit  $ stream

genToTransmit :: IO (Word8 -> Word8)
genToTransmit = do
    gen <- newStdGen
    return (transmit gen)
