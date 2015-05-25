import           Control.Monad (replicateM)
import qualified Criterion
import qualified Criterion.Main as Criterion
import qualified Data.ByteString as BS
import qualified Data.Digest.CRC32 as C
import qualified Data.Digest.Pure.CRC32 as Pure
import           Data.Functor ((<$>))
import qualified System.Random.MWC as MWC

randomByteStrings :: IO [(Int, BS.ByteString)]
randomByteStrings = MWC.withSystemRandom $ \rand -> do
  let randBS :: Int -> IO (Int, BS.ByteString)
      randBS l = do
        bs <- BS.pack <$> replicateM l (MWC.uniform rand)
        return (l, bs)
  mapM randBS [10, 100, 1000, 10000, 100000, 1000000, 10000000]

main :: IO ()
main = Criterion.defaultMain [
  Criterion.env randomByteStrings $ \ bss ->
    Criterion.bgroup "CRC32" $ map bench bss
  ]
  where
    bench (l, bss) = Criterion.bgroup (showBytes l)
      [ Criterion.bench "Haskell" $ Criterion.nf Pure.crc32 bss
      , Criterion.bench "C" $ Criterion.nf C.crc32 bss
      ]

    showBytes n
      | n < 1000 = show n ++ "B"
      | n < 1000000 = show (n `div` 1000) ++ "kB"
      | otherwise = show (n `div` 1000000) ++ "MB"
