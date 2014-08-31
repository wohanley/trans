import Data.ByteString.Lazy as BS
import Trans
import System.Random

main = do
    stream <- BS.getContents
    gen <- getStdGen
    BS.putStr . BS.map (transmit gen) $ stream
