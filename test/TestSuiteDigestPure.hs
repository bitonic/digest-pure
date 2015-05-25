{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Adler32 as A
import qualified Data.Digest.CRC32 as C
import qualified Data.Digest.Pure.Adler32 as AP
import qualified Data.Digest.Pure.CRC32 as CP
import Data.Functor ((<$>))
import System.Exit
import Test.QuickCheck
import Text.Printf

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary
  shrink xs = B.pack <$> shrink (B.unpack xs)

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary
  shrink xs = BL.pack <$> shrink (BL.unpack xs)

main :: IO ()
main = do
  results <- mapM (\(s, a) -> printf "%-25s: " s >> a) tests
  if all succesful results then
    exitSuccess
  else
    exitFailure

succesful :: Result -> Bool
succesful (Success _ _ _) = True
succesful _               = False

tests :: [([Char], IO Result)]
tests = [
  ("adler32CompareZlib",   quickCheckResult adler32CompareZlib),
  ("adler32Update",        quickCheckResult adler32Update),
  ("crc32CompareZlib",     quickCheckResult crc32CompareZlib),
  ("crc32CompareZlibLazy", quickCheckResult crc32CompareZlibLazy),
  ("crc32Update",          quickCheckResult crc32Update),
  ("crc32Unaligned",       quickCheckResult crc32Unaligned)
 ]

adler32CompareZlib :: B.ByteString -> Bool
adler32CompareZlib str =
  AP.adler32 str == A.adler32 str

adler32Update :: B.ByteString -> B.ByteString -> Bool
adler32Update str1 str2 =
  AP.adler32 strConcat == AP.adler32Update (AP.adler32 str1) str2
  where
    strConcat = B.append str1 str2

crc32CompareZlib :: B.ByteString -> Bool
crc32CompareZlib str =
  CP.crc32 str == C.crc32 str

crc32CompareZlibLazy :: BL.ByteString -> Bool
crc32CompareZlibLazy str =
  CP.crc32 str == C.crc32 str

crc32Update :: B.ByteString -> B.ByteString -> Bool
crc32Update str1 str2 =
  CP.crc32 strConcat == CP.crc32Update (CP.crc32 str1) str2
  where
    strConcat = B.append str1 str2

crc32Unaligned :: B.ByteString -> Gen Bool
crc32Unaligned str0 = do
  n <- choose (1, 3)
  let str = B.drop n str0
  return $ CP.crc32 str == C.crc32 str
