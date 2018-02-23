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




test_1BAD_J = testVar3File result1A "resultBAD1" "resultJ1" layoutTriples
test_2BAD_J = testVar3File result2A "resultBAD2" "resultJ2" layoutTriples
test_3BAD_J = testVar3File result3A "resultBAD3" "resultJ3" layoutTriples
test_4BAD_J = testVar3File result4A "resultBAD4" "resultJ4" layoutTriples
test_5BAD_J = testVar3File result5A "resultBAD5" "resultJ5" layoutTriples
test_6BAD_J = testVar3File result6A "resultBAD6" "resultJ6" layoutTriples
----test_7BAD_J = testVar3File result7A "resultBAD7" "resultJ7" layoutTriples
test_8BAD_J = testVar3File result8A "resultBAD8" "resultJ8" layoutTriples
test_9BAD_J = testVar3File result9A "resultBAD9" "resultJ9" layoutTriples
test_10BAD_J = testVar3File result10A "resultBAD10" "resultJ10" layoutTriples




