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
module CoreNLP.Doc2ToLinear_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness
import CoreNLP.Doc2ToLinear
import qualified NLP.Corpora.Conll  as Conll

toLin ::   (Doc11 Conll.POStag) ->  [DocAsList Conll.POStag]
toLin   =  linearize Conll.undefConll

instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness [DocAsList Conll.POStag] where


test_c = testFile2File "short1.doc11" "short1.lin" toLin


