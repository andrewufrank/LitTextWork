-----------------------------------------------------------------------------
--
-- Module      :   for automatic test
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.Strings
import           Test.Framework
import {-@ HTF_TESTS @-} LitText.Foundation.ServerNames_test
import {-@ HTF_TESTS @-} LitText.Foundation.TextDescriptor_test

main =  do
    twoOut
    putStrLn "LitText.Foundation \n"
    -- r <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r)
    r <- htfMain htf_importedTests
    -- putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
