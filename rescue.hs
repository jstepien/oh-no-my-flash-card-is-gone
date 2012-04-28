import Prelude hiding (catch)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, when)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents)
import System.Process (runInteractiveCommand, waitForProcess)

type Offset = Int64

spawn :: Offset -> ByteString -> IO ()
spawn off bs = catch (forM_ [rescue, exif] run) nvm
  where
    rescue = "djpeg | convert - -quality 90% " ++ file
    exif = "exiftool -TagsFromFile /dev/stdin -all:all " ++ file
    file = show off ++ ".jpeg"
    input = BS.take (10 * 1000 * 1000) bs
    nvm :: IOException -> IO ()
    nvm _ = return ()
    run cmd = do
      handles <- runInteractiveCommand cmd
      let (hin, hout, herr, child) = handles
      catch (BS.hPutStr hin input) nvm
      hClose hin
      forM_ [hout, herr] $ \h -> hGetContents h >>= putStr
      code <- waitForProcess child
      when (code /= ExitSuccess) (fail "Non-zero exit status")

loop :: ByteString -> [Offset] -> Offset -> IO ()
loop _ [] _ = return ()
loop bs (off:offsets) prev = do
  spawn off bs
  loop (BS.drop (off - prev) bs) offsets off

main :: IO ()
main = do
  offsetsFile <- getArgs >>= readFile . head
  input <- BS.getContents
  let offsets = map read $ lines offsetsFile :: [Offset]
  loop input offsets 0
