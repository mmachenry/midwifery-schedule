module Main (main) where

import qualified Schedule (main)

main :: IO ()
main = do
  Schedule.schedule
