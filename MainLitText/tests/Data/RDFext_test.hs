-----------------------------------------------------------------------------
--
-- Module      :   a test for automatic check of treetagger call
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.RDFext_test (
    module Data.RDFext.Codes_test
    ) where


--import Uniform.Strings
import           Test.Framework
import {-@ HTF_TESTS @-}  Data.RDFext.Codes_test
--import {-@ HTF_TESTS @-}  Data.RDFext.FileTypes_test

main =  do
    putStrLn " Data.RDF.Extension\n"
    -- r <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r)
    r <- htfMain htf_importedTests
    -- putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
