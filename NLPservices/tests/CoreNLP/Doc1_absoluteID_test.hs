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

module CoreNLP.Doc1_absoluteID_test  -- (openMain, htf_thisModuelsTests)
     where

import           Test.Framework
import Uniform.Test.TestHarness
import           Uniform.Strings
--import Uniform.FileIO
----import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.Lazy.UTF8 as B
--import Data.Aeson (eitherDecode)
--
import CoreNLP.Doc1_absoluteID
--
--import Data.Aeson.Encode.Pretty
--import Data.Aeson
--import GHC.Exts
import qualified NLP.TagSets.Conll  as Conll


instance ShowTestHarness (Doc11 Conll.POStag) where
instance ShowTestHarness (Doc1 Conll.POStag) where

progName = "nlpservices"
test_c = testFile2File progName "short1.doc3" "short1.doc4" to11op


