module Main (main) where

import qualified Mates.App as App
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= App.mainWith
