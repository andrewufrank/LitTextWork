-----------------------------------------------------------------------------
--
-- Module      :  Parser . NLP Vocabulary
-- Copyright   :  andrew u frank -
--
-- | the list of properties and types used to describe the NLP results
-- addition to the list of Treebank codes imported and exported here
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.NLPvocabulary_test  where

import           CoreNLP.Defs0
import           Data.RDF.Extension      --(PartURI, RDFproperty)
import           Text.Printf             (printf)
import           Uniform.Strings         hiding ((<|>))
import Parser.TextDescriptor hiding ((</>)) -- from Foundation
import Producer.Servers (rdfBase, vocabularyBase)  -- from Foundation
import Parser.NLPvocabulary

test_emtpy = assertEqual 1 1

