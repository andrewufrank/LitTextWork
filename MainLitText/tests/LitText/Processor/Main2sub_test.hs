---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the common process to producing the lit and nlp triples
-- could test initially if the services (treetagger, fuseki, corenlp are available
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Processor.Main2sub_test  where

import           Test.Framework
import Parser.ReadMarkupAB
import           BuchCode.MarkupText
import LitText.Parser.ProduceLayout
import           LitText.Lines2para.Lines2para hiding ((</>))
import           LitText.Lines2para.Lines2ignore
import           LitText.Parser.ProduceLit
import           LitText.Parser.ProduceNLP
import           Uniform.FileIO (when, errorT)
--import           Uniform.Strings
import LitText.Lines2para.HandleLayout
import LitTypes.UtilsParseArgs ( LitTextFlags (..) )
import Processor.Main2sub



