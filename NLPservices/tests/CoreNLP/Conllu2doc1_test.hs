-----------------------------------------------------------------------------
--
-- Module      :  a test for all of CoreNLP
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts
    , FlexibleInstances      #-}
--{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module CoreNLP.Conllu2doc1_test  -- (openMain, htf_thisModuelsTests)
     where

import           Test.Framework
import Uniform.Test.TestHarness
import           Uniform.Strings
import CoreNLP.CoreNLP
import qualified NLP.TagSets.Conll  as Conll
import LitTypes.ServerNames (rdfBase)


-- instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness NTtext where

progName = "nlpservices"
--test_all :: IO ()
--test_all = testFile2File (progName) "short1.json" "short1.nt" (json2NT rdfBase)


