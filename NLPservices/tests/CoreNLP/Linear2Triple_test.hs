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

toLin ::   [DocAsList Conll.POStag] -> [DocAsTriple Conll.POStag]
toLin ds  =  concat r
    where r =  map (makeTriple rdfBase) ds :: [[DocAsTriple Conll.POStag]]

instance ShowTestHarness [DocAsList Conll.POStag] where
--instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness [DocAsTriple Conll.POStag] where


test_c = testFile2File "short1.lin" "short1.trips" toLin


