module Confer.CLI.UI where

import Data.ByteString
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Layoutz

printProgress
  :: Console :> es
  => String
  -> Double
  -> Eff es ()
printProgress message percentage = do
  let element = inlineBar message percentage
  Console.putStr (overwriteNthLine 1 (T.encodeUtf8 $ T.pack $ render element))
  where
    clearLine :: StrictByteString
    clearLine = "\x1b[2K"
    moveLineUp :: Int -> StrictByteString
    moveLineUp n = "\x1b[" <> T.encodeUtf8 (T.pack (show n)) <> "A"
    moveLineDown :: Int -> StrictByteString
    moveLineDown n = "\x1b[" <> T.encodeUtf8 (T.pack (show n)) <> "B"
    pos1 :: StrictByteString
    pos1 = "\r"
    overwriteNthLine :: Int -> StrictByteString -> StrictByteString
    overwriteNthLine n str = pos1 <> moveLineUp n <> clearLine <> str <> moveLineDown n <> pos1
