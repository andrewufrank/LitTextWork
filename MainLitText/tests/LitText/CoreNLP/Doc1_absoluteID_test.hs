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

module LitText.CoreNLP.Doc1_absoluteID_test  -- (openMain, htf_thisModuelsTests)
     where

import           Test.Framework
import Uniform.Test.TestHarness
import           Uniform.Strings
import LitText.CoreNLP.Doc1_absoluteID
import qualified NLP.TagSets.Conll  as Conll


instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness (Doc1 Conll.POStag) where

--instance Zeros Conll.POStag where zero = Conll.Unk

progName = "nlpservices"
test_c :: IO ()
test_c = test1File progName "short1.doc3" "short1.doc4"
                (to11op Conll.undefPOS)


