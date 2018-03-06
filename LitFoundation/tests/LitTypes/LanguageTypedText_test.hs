-----------------------------------------------------------------------------
--
-- Module      :  Language Typed Text
-- Copyright   :  andrew u frank -
--
-- | a text type which has as a type paramter the language


-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.LanguageTypedText_test where

import           Test.Framework
--import Uniform.TestHarness (testVar3File)
import Uniform.Zero (Zeros (..))
import Uniform.Strings
import Uniform.Error (undef)
import Data.RDF.Extension -- (LanguageCode (..))
--import CoreNLP.Defs0
--import Parser.TextDescriptor
--import NLP.Types.Tags
--import Parser.NLPvocabulary  -- from Foundation
import Parser.LanguageTypedText


