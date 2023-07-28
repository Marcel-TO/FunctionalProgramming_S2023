import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get (isEmpty, getWord8, Get, runGet)
import Data.Binary (Word8)

main = do
    pic <- BS.readFile "./advanced/Urlaubsphoto.png.crypt"
    BS.writeFile "./advanced/Urlaubsphoto.png" (BS.pack (runGet decrypt pic))

decrypt :: Get [Word8]
decrypt = do
    empty <- isEmpty
    if empty
        then return []
        else do
            first <- getWord8 -- first :: Word8
            second <- getWord8 -- second :: Word8
            xs <- decrypt -- xs :: [Word8]
            return ((min first second) : xs) -- return verpackt in Get