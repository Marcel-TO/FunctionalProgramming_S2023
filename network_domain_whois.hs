import Network.Simple.TCP (connect, send, recv)
import qualified Data.ByteString as BS

main = do
	putStr "Domain name: "
	hFlush stdout --durch puffern kann der string vllt erst nach getline ausgegeben werden
	domain <- BS.getLine -- returnt bytestring
	connect "whois.nic.at" "43" $ \(socket, _) -> do
		send socket domain
		mbs <- recv socket 4096
		case mbs of
			Nothing -> return()
			Just bs -> BS.putStrLn bs