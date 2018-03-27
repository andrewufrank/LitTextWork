-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module CoreNLP.Linear2Triple_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness
import CoreNLP.Linear2Triple
import qualified NLP.TagSets.Conll  as Conll
import Data.RDFext.Extension as RDF
import LitTypes.ServerNames (rdfBase)


instance ShowTestHarness [DocAsList Conll.POStag] where
--instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness [DocAsTriple ] where
instance ShowTestHarness [Triple ] where

progName = "nlpservices"
test_c = testFile2File progName "short1.lin5" "short1.trips6" (toTriple rdfBase)

test_d = testFile2File progName "short1.trips6" "short1.nt" toNT


test_intercalate1 = assertEqual (Just "doc11/S000001/T006")
             (intercalate' "/" . reverse $ ts1)

ts1 = [ "T006" , "S000001" , "doc11" ] :: [Text]
