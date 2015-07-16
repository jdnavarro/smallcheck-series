{-# LANGUAGE CPP #-}
module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
#if !MIN_VERSION_doctest(0,10,1)
main = doctest . ([ "-idist/build/autogen"
                  , "-optP-include"
                  , "-optPdist/build/autogen/cabal_macros.h"
                  ] ++) =<< glob "Test/**/*.hs"
#else
main = doctest =<< glob "Test/**/*.hs"
#endif
