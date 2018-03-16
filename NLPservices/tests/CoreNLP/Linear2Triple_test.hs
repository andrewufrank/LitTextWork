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
import qualified NLP.Corpora.Conll  as Conll

toLin ::   [DocAsList Conll.POStag] -> [DocAsTriple ]
toLin ds  =  concat r
    where r =  map (makeTriple rdfBase) ds :: [[DocAsTriple ]]

instance ShowTestHarness [DocAsList Conll.POStag] where
--instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness [DocAsTriple ] where

progName = "nlpservices"
test_c = testFile2File progName "short1.lin" "short1.trips" toLin

test_intercalate1 = assertEqual (Just "doc11/S000001/T006")
             (intercalate' "/" . reverse $ ts1)

ts1 = [ "T006" , "S000001" , "doc11" ] :: [Text]
