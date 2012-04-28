module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)

type Offset = Int64

scan :: Int64 -> ByteString -> Maybe (Offset, Offset, ByteString)
scan off bs = off `seq` case BS.unpack chunk of
  (0xff:0xd8:_) -> Just (off, off + patternSize, BS.drop patternSize bs)
  (_:_) -> scan (off + 1) (BS.drop 1 bs)
  [] -> Nothing
  where
    patternSize = 2
    chunk = BS.take patternSize bs

loop :: ByteString -> Offset -> IO ()
loop bs off =
  case scan off bs of
    Nothing -> return ()
    Just (beginning, newOff, rest) -> do
      print beginning
      loop rest newOff

main :: IO ()
main = do
  input <- BS.getContents
  loop input 0
