-----------------------------------------------------------------------------
--
-- Module      :   a test for automatic check of treetagger call
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.Strings
import           Test.Framework
--import {-@ HTF_TESTS @-} BuchCode.MarkupText   -- is in Test of BuchCode
import {-@ HTF_TESTS @-} Lines2para.HandleLayout
import {-@ HTF_TESTS @-} Lines2para.Lines2ignore
import {-@ HTF_TESTS @-} Lines2para.Lines2para

main =  do
    putStrLn "Lit Text Test.hs:\n"
    -- r <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r)
    r <- htfMain htf_importedTests
    putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
