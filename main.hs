import Data.ByteString.Lazy as BS
import Trans
import System.Random
import GHC.Word

main = do
    stream <- BS.getContents
    words <- sequence ((Prelude.map transform) . unpack $ stream)
    BS.putStr . pack $ words

transform :: Word8 -> IO Word8
transform source = do
    loadedTransmit <- genToTransmit
    return (loadedTransmit source)

genToTransmit :: IO (Word8 -> Word8)
genToTransmit = do
    gen <- newStdGen
    return (transmit gen)
