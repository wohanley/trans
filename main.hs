import Data.ByteString.Lazy as BS
import Trans
import System.Random
import GHC.Word

main = do
    stream <- BS.getContents
    mapM_ transform (unpack stream)

transform :: Word8 -> IO ()
transform source = do
    loadedTransmit <- genToTransmit
    print . loadedTransmit $ source

genToTransmit :: IO (Word8 -> Word8)
genToTransmit = do
    gen <- newStdGen
    return (transmit gen)
