module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = doctest . ([ "-idist/build/autogen"
                  , "-optP-include"
                  , "-optPdist/build/autogen/cabal_macros.h"
                  ] ++) =<< glob "Test/**/*.hs"
