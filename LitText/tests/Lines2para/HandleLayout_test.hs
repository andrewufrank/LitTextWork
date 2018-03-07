-----------------------------------------------------------------------------
--
-- Module      :  Parser . HandleLayout
-- Copyright   :  andrew u frank -
--
-- |  encode the layout on pages (lines, pages)
-- should pages become items (for rdf)?
-- line numbers are not including page number lines
-- should ignore lines not be put in rdf (yes - avoid pre and post stuff from gutenberg)
-- does not handle language yet
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Lines2para.HandleLayout_test
      where

import Test.Framework
--import BuchCode.MarkupText
--import           Data.List.Split
-- todo strings
--import           Uniform.Error
--import Uniform.Zero
-- todo include zero  in error and strings
-- TODO string s
--import Data.List (nub)
--import           Text.Printf         (printf)
import Uniform.TestHarness
--import Parser.TextDescriptor -- (TZ (..), tlline, tlpage , TextType (..))
import Lines2para.HandleLayout

--paragraphs2TZlayout :: [TextZeile] -> [TZ]  -- test BA -> BB


----test_0BA_BB = testFile2File "resultBA0" "resultBB0" paragraphs2TZlayout
test_1BA_BB = testFile2File "resultBA1" "resultBB1" paragraphs2TZlayout
test_2BA_BB = testFile2File "resultBA2" "resultBB2" paragraphs2TZlayout
test_3BA_BB = testFile2File "resultBA3" "resultBB3" paragraphs2TZlayout
test_4BA_BB = testFile2File "resultBA4" "resultBB4" paragraphs2TZlayout
test_5BA_BB = testFile2File "resultBA5" "resultBB5" paragraphs2TZlayout
test_6BA_BB = testFile2File "resultBA6" "resultBB6" paragraphs2TZlayout
test_8BA_BB = testFile2File "resultBA8" "resultBB8" paragraphs2TZlayout
test_9BA_BB = testFile2File "resultBA9" "resultBB9" paragraphs2TZlayout
test_10BA_BB = testFile2File "resultBA10" "resultBB10" paragraphs2TZlayout
test_11BA_BB = testFile2File "resultBA11" "resultBB11" paragraphs2TZlayout
test_12BA_BB = testFile2File "resultBA12" "resultBB12" paragraphs2TZlayout


