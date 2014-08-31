import Data.ByteString.Lazy as BS
import Trans

main = do
    stream <- BS.getContents
    BS.putStr . BS.map transmit $ stream
