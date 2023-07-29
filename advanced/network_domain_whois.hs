import Network.Simple.TCP (connect, send, recv, withSocketsDo)
import qualified Data.ByteString as BS
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "Domain name: "
  hFlush stdout -- durch puffern kann der string vllt erst nach getline ausgegeben werden
  domain <- BS.getLine
  connect "whois.nic.at" "43" $ \(socket, _) -> do
    send socket domain
    recvloop socket
  where
    recvloop socket = do
      mbs <- recv socket 4096
      case mbs of
        Just bs -> BS.putStr bs >> recvloop socket
        Nothing -> return ()