module Main (main) where

import Data.ByteString.Lazy (getContents)
import Frontend.Lexer (SpannedToken (..), Token (..), alexMonadScan, runAlex)

scanMany :: LByteString -> Either String [SpannedToken]
scanMany input = runAlex input go
  where
    go = do
      st <- alexMonadScan
      if st.stToken == TEof
        then pure [st]
        else (st :) <$> go

main :: IO ()
main = print . scanMany =<< getContents
