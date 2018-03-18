-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
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
module CoreNLP.DocNLP_0or1_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness
import           Uniform.Strings
import qualified NLP.Corpora.Conll  as Conll
--
import CoreNLP.DocNLP_0or1


instance ShowTestHarness (Doc1 Conll.POStag) where
instance ShowTestHarness Doc2 where

progName = "nlpservices"
test_B = testFile2File progName "short1.doc2" "short1.doc3" to1op


