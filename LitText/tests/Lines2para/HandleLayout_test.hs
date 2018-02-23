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



----test_0BA_BAC = testFile2File "resultBA0" "resultBAC0" paragraphs2TZlayout
test_1BA_BAC = testFile2File "resultBA1" "resultBAC1" paragraphs2TZlayout
test_2BA_BAC = testFile2File "resultBA2" "resultBAC2" paragraphs2TZlayout
test_3BA_BAC = testFile2File "resultBA3" "resultBAC3" paragraphs2TZlayout
test_4BA_BAC = testFile2File "resultBA4" "resultBAC4" paragraphs2TZlayout
test_5BA_BAC = testFile2File "resultBA5" "resultBAC5" paragraphs2TZlayout
test_6BA_BAC = testFile2File "resultBA6" "resultBAC6" paragraphs2TZlayout
test_8BA_BAC = testFile2File "resultBA8" "resultBAC8" paragraphs2TZlayout
test_9BA_BAC = testFile2File "resultBA9" "resultBAC9" paragraphs2TZlayout
test_10BA_BAC = testFile2File "resultBA10" "resultBAC10" paragraphs2TZlayout
test_11BA_BAC = testFile2File "resultBA11" "resultBAC11" paragraphs2TZlayout
test_12BA_BAC = testFile2File "resultBA12" "resultBAC12" paragraphs2TZlayout


