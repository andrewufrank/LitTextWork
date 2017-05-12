-----------------------------------------------------------------------------
--
-- Module      :   a test for automatic check of treetagger call
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

-- all must have {-@ HTF_TESTS @-}

import Uniform.Strings
import           Test.Framework
--import {-@ HTF_TESTS @-} Parser.ReadMarkupAB
--import {-@ HTF_TESTS @-} BuchCode.MarkupText
--import   {-@ HTF_TESTS @-} Lines2para.Lines2paraTests
--import   {-@ HTF_TESTS @-} Parser.ProduceLit
--import   {-@ HTF_TESTS @-} Parser.ProduceNLP
------ makes call to NLP
--import   {-@ HTF_TESTS @-} Parser.ProduceNLPtriples
----import   {-@ HTF_TESTS @-} CoreNLP.Snippets2nt  -- no tests?
----import {-@ HTF_TESTS @-} Main2sub
--import {-@ HTF_TESTS @-} Parser.Main2subTest
import  {-@ HTF_TESTS @-} Processor.ProcessAll

main =  do
    putStrLn "Lit Text Test.hs:\n"
    -- r <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r)
    -- r <- htfMain htf_importedTests
    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    -- putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
-- -- main =  do  -- with tests in other modules
-- --     putStrLn "HTF ExampleTest.hs:\n"
-- --     p <- htfMain htf_importedTests
-- --     putStrLn ("HTF end StringConversion.hs test:\n" ++ show p)
-- --     return ()
--
-- -- start function name with test for a tests with given results
-- test_nonEmpty = do assertEqual [1] (myReverse [1])
--                    assertEqual [3,2,1] (myReverse [1,2,3])
--
-- test_empty = assertEqual ([] :: [Int]) (myReverse [])
--
-- -- start function name with prop for testing with random generated values
-- prop_reverse :: [Int] -> Bool
-- prop_reverse xs = xs == myReverse (myReverse xs)
--
-- -- a test which failed can be repeated
-- prop_reverseReplay =
--   withQCArgs (\a -> a { replay = read
--     "Just (TFGenR 000034A28CA0BA65000000003B9ACA00000000000000E1F70000000000\
--         \000000 0 127 7 0,6)" })
--   prop_reverse
