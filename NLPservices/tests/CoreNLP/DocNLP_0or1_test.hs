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
--import Uniform.FileIO
----import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.Lazy.UTF8 as B
--import Data.Aeson (eitherDecode)
import qualified NLP.Corpora.Conll  as Conll
--
import CoreNLP.DocNLP_0or1
--
--import Data.Aeson.Encode.Pretty
--import Data.Aeson
--import GHC.Exts

to1op :: Doc2  ->   (Doc1 Conll.POStag)
to1op f =  convertTo1 Conll.undefConll f

instance ShowTestHarness (Doc1 Conll.POStag) where
instance ShowTestHarness Doc2 where


test_B = testFile2File "short1.doc2" "short1.doc1" to1op


