-----------------------------------------------------------------------------
--
-- Module      :  Defs0
--
-- | the data types etc necessary to read the XML files
-- produced by Stanford CoreNLP
-- captures all info in the XML output but does no furthre processing
-- nor reading (except for the id)
-- the structure is a nested data structure, exactly parallel to the produced xml

-- optional fields are lists .. to conform to HXT
-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveAnyClass
    #-}

module CoreNLP.Defs0_test   where

import              Uniform.Strings
import Uniform.Zero
import   NLP.Corpora.Conll
import CoreNLP.Defs0
import              CoreNLP.DEPcodes
import              CoreNLP.NERcodes
-- import           Text.XML.HXT.Core       hiding (when)


test_emtpy = assertEqual 1 1
