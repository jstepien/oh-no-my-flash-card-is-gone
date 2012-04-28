import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
  (offset:count:_) <- liftM (map read) getArgs
  BS.interact (BS.take count . BS.drop offset)
