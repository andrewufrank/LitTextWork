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
import           Data.Char               (toLower)
--import           Data.RDF  ()
--import Data.RDF.Triple2text (triple2text)
--import           Data.RDF.Extension
--import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
--import           Parser.TextDescriptor  hiding ((</>))
import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB_test    -- result1A etc.
--import Lines2para.HandleLayout
    -- (RDFtypes (..), RDFproperties (..), TZ (..)
--      , TextDescriptor, PartURI, RDFsubj, Triple) -- TZ
--import Lines2para.Lines2ignore
--import Lines2para.Lines2para -- hiding ((</>))
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
import Uniform.TestHarness
import Producer.Servers (rdfBase)  -- from Foundation
import Parser.TextDescriptor hiding ((</>)) -- from Foundation
import Parser.NLPvocabulary
import Parser.ProduceLayout


layoutTriples ::  TextDescriptor -> [TZ1] -> Text  -- test BAD -> J

--layoutTriples textstate =  unlines' .  map triple2text . produceLayoutTriples textstate
-- too expensive to map triple2text (at least on oporto)
layoutTriples textstate =  unlines' .  map showT . produceLayoutTriples textstate


test_1C_J = test3File "result1A" "resultC1" "resultJ1" layoutTriples
test_2C_J = test3File "result2A" "resultC2" "resultJ2" layoutTriples
test_3C_J = test3File "result3A" "resultC3" "resultJ3" layoutTriples
test_4C_J = test3File "result4A" "resultC4" "resultJ4" layoutTriples
test_5C_J = test3File "result5A" "resultC5" "resultJ5" layoutTriples
test_6C_J = test3File "result6A" "resultC6" "resultJ6" layoutTriples
----test_7C_J = test3File result7A "resultC7" "resultJ7" layoutTriples
test_8C_J = test3File "result8A" "resultC8" "resultJ8" layoutTriples
test_9C_J = test3File "result9A" "resultC9" "resultJ9" layoutTriples
test_10C_J = test3File "result10A" "resultC10" "resultJ10" layoutTriples




