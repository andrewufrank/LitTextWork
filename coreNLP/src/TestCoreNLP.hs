-----------------------------------------------------------------------------
--
-- Module      : currently no tests in this package
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.Strings
import           Test.Framework
import {-@ HTF_TESTS @-} CoreNLP.DepCodes
--import {-@ HTF_TESTS @-} CoreNLP.DepCodes

main =  do
    putStrLn "Lit Text Test.hs:\n"
    r <- htfMain htf_importedTests
    putStrLn ("other tests t:\n" ++ show r)
    return ()
