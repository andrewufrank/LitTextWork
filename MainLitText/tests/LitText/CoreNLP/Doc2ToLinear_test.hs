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
module LitText.CoreNLP.Doc2ToLinear_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness
--import LitText.CoreNLP.Doc2ToLinear
import qualified Data.Text  as T
import qualified NLP.TagSets.Conll  as Conll
import         LitText.CoreNLP.CoreNLP


instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness [DocAsList Conll.POStag] where

progName = "nlpservices"
test_c = test1File progName "short1.doc4" "short1.lin5"
                (toLin Conll.undefPOS)

test_2spaces = assertEqual s2r (T.replace "  " " " s2)

s2 = "When  the  uncle  came  into  the  room,  he  carried  a  book. "
s2r = "When the uncle came into the room, he carried a book. "
