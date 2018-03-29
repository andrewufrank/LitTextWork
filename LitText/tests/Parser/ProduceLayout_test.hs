-----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceText
-- Copyright   :  andrew u frank -
--
-- | not used when not producing text included
-- works with TZ1, language is marked
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Parser.ProduceLayout_test where

import           Test.Framework
--import           Data.Char               (toLower)
--import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB_test    -- result1A etc.
--import           Text.Printf         (printf)
--import           Uniform.Error           (errorT)
import Uniform.Test.TestHarness
--import Producer.Servers (rdfBase)  -- from Foundation
--import Parser.TextDescriptor hiding ((</>)) -- from Foundation
--import Parser.NLPvocabulary
import Parser.ProduceLayout


layoutTriples ::  TextDescriptor -> [TZ1] -> Text  -- test C -> J

--layoutTriples textstate =  unlines' .  map triple2text . produceLayoutTriples textstate
-- too expensive to map triple2text (at least on oporto)
layoutTriples textstate =  unlines' .  map showT . produceLayoutTriples textstate

instance ShowTestHarness TextDescriptor
instance ShowTestHarness [TZ1]


test_1C_J = test2File progName "resultA1" "resultC1" "resultJ1" layoutTriples
test_2C_J = test2File progName "resultA2" "resultC2" "resultJ2" layoutTriples
test_3C_J = test2File progName "resultA3" "resultC3" "resultJ3" layoutTriples
test_4C_J = test2File progName "resultA4" "resultC4" "resultJ4" layoutTriples
test_5C_J = test2File progName "resultA5" "resultC5" "resultJ5" layoutTriples
test_6C_J = test2File progName "resultA6" "resultC6" "resultJ6" layoutTriples
----test_7C_J = test2File progName resultA7 "resultC7" "resultJ7" layoutTriples
test_8C_J = test2File progName "resultA8" "resultC8" "resultJ8" layoutTriples
test_9C_J = test2File progName "resultA9" "resultC9" "resultJ9" layoutTriples
test_10C_J = test2File progName "resultA10" "resultC10" "resultJ10" layoutTriples




