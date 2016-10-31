{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.Compression.GZip (compress)
import           Control.Applicative    ((<|>))
import           Control.Monad          (forM_)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as B
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as H
import           Data.Int
import           Data.Text.IO           (putStrLn)
import           Formatting             (float, int, sformat, stext, (%))
import           Prelude                hiding (putStrLn)
import           System.IO              (IOMode (..), openFile)

import           Debug.Trace

type WordSize = Int64
type WordProbas = HashMap ByteString Double

slice :: Int64 -> Int64 -> ByteString -> ByteString
slice start len = B.take len . B.drop start

wordProbas :: WordSize -> ByteString -> WordProbas
wordProbas n text = fmap calcProba $ foldr countWord H.empty [0..blocksCount]
  where blocksCount = B.length text - n
        calcProba x = fromIntegral x / fromIntegral blocksCount
        countWord offset = H.alter incOr1 $ slice offset n text
        incOr1 val = fmap (+1) val <|> Just 1

entropy :: WordSize -> ByteString -> Double
entropy n text = - sum summands / fromIntegral n
  where wordCount = fromIntegral $ B.length text - n
        summands = map (\x -> x * logBase 2 x) probas
        probas = H.elems $ wordProbas n text

main :: IO ()
main = do
  h <- openFile "alice29.txt" ReadMode
  file <- B.hGetContents h

  forM_ [1..5] $ \n -> do
    let curEntropy = entropy n file
        compRatio = 8.0 / curEntropy
    putStrLn $ sformat ("n = "%int%": entropy "%float%"; compression ratio "%float) n curEntropy compRatio

  let cfile = compress file
      csize = B.length cfile
      fsize = B.length file

  putStrLn $ sformat ("Original file size: "%int) fsize
  putStrLn $ sformat ("GZip compressed file size: "%int) csize
  putStrLn $ sformat ("Compression ratio: "%float) $ fromIntegral fsize / fromIntegral csize

  let half1 = B.take (fsize `div` 2) file
      half2 = B.drop (fsize `div` 2) file

  forM_ [1..4] $ \n -> do
    let en1 = entropy n half1
        en2 = entropy n half2
    putStrLn $ sformat ("n = "%int%": half1 "%float%"; half2 "%float) n en1 en2
