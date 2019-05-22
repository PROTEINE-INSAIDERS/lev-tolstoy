import Data.Binary.Get
import Data.ByteString.Lazy as BL
import Data.Word

get2Words :: Get Word64
get2Words = (+) <$> getWord64host <*> getWord64host

main :: IO ()
main = do 
    let bs = BL.replicate 16 0
        r  = runGet get2Words bs
    print r