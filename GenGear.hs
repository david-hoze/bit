-- | Generate gear hash table entries deterministically.
-- For each byte i in [0..255], compute MD5("gear" ++ show i),
-- take first 8 bytes as little-endian Word64.
module Main where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Word (Word64)
import Data.Bits (shiftL, (.|.))
import Text.Printf (printf)

toWord64LE :: BS.ByteString -> Word64
toWord64LE bs = foldl (\acc (i, b) -> acc .|. (fromIntegral b `shiftL` (i * 8))) 0
  (zip [0..7] (BS.unpack (BS.take 8 bs)))

main :: IO ()
main = do
  let entries = [ toWord64LE (MD5.hash (pack ("gear" ++ show i))) | i <- [0..255 :: Int] ]
  putStrLn "gearTable = listArray (0, 255)"
  let grouped = groupsOf 4 entries
  mapM_ (\(gi, grp) -> do
    let prefix = if gi == 0 then "  [ " else "  , "
    let formatted = map (printf "0x%016x" :: Word64 -> String) grp
    putStrLn (prefix ++ intercalate ", " formatted)
    ) (zip [0::Int ..] grouped)
  putStrLn "  ]"

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (a, b) = splitAt n xs in a : groupsOf n b

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs
