-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines
-- Copyright   :  andrew u frank -
--
-- |  grouping the lines to paragraphs  - completes the parsing
-- TextZeilen is reading in , TZ is a conversion of TextZeilen (no IO)
-- works only on text lines
-- unparse the internal TZ representation and produce a tile to compare with the
--original txt file
-- does not show the page numbers ???
-- seitenzahlen must be numbers (not alpha) - is used to parse!
-- .ende is necessary to distribute page numbers!
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lines2para.Lines2para_test  where

import           Uniform.Error
import Uniform.FileIO
import Data.List (nub)
import           Test.Framework
import Uniform.Test.TestHarness
import LitText.Foundation -- (ParaNum (..), unparaNum)
--import Lines2para.Lines2para

instance ShowTestHarness [TZ1]
instance ShowTestHarness [TZ2]

progName = "tests"
--paragraphsTZ2TZ2 :: [TZ1] -> [TZ2]  -- test C -> CA
----test_0BA_BAC = test1File progName "resultBA0" "resultBAC0" paragraphs2TZpara
test_1C_CA = test1File progName "resultC1" "resultCA1" paragraphsTZ2TZ2
test_2C_CA = test1File progName "resultC2" "resultCA2" paragraphsTZ2TZ2
test_3C_CA = test1File progName "resultC3" "resultCA3" paragraphsTZ2TZ2
test_4C_CA = test1File progName "resultC4" "resultCA4" paragraphsTZ2TZ2
test_5C_CA = test1File progName "resultC5" "resultCA5" paragraphsTZ2TZ2
test_6C_CA = test1File progName "resultC6" "resultCA6" paragraphsTZ2TZ2
test_8C_CA = test1File progName "resultC8" "resultCA8" paragraphsTZ2TZ2
test_9C_CA = test1File progName "resultC9" "resultCA9" paragraphsTZ2TZ2
test_10C_CA = test1File progName "resultC10" "resultCA10" paragraphsTZ2TZ2
test_11C_CA = test1File progName "resultC11" "resultCA11" paragraphsTZ2TZ2
test_12C_CA = test1File progName "resultC12" "resultCA12" paragraphsTZ2TZ2



