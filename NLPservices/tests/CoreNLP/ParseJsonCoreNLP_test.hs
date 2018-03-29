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
{-# LANGUAGE OverloadedStrings
    , StandaloneDeriving
    , DeriveAnyClass    #-}
module CoreNLP.ParseJsonCoreNLP_test
     where


import           Test.Framework
import Uniform.Test.TestHarness
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (eitherDecode)
import qualified NLP.TagSets.Conll  as Conll

import CoreNLP.ParseJsonCoreNLP

import Data.Aeson.Encode.Pretty
import Data.Aeson
import GHC.Exts

deriving instance ToJSON (Conll.POStag)

test_toJson = assertEqual objVBG (toJSON (Conll.VBG))

objVBG = String "VBG"



progName = "nlpservices"
test_A = test1File progName "short1.json" "short1.doc2" decodeDoc2op

instance ShowTestHarness Doc2 where
instance ShowTestHarness a => ShowTestHarness (ErrOrVal a) where

instance (Zeros a) => Zeros (ErrOrVal a) where zero = Right zero

restest = zero :: Doc2


showStartJson = s2t . take 100 . B.toString

runErrorFromEither :: (Show s, CharChains s) => Either s a -> ErrIO a
runErrorFromEither (Left s) = throwErrorT ["runErrorFromEither", (toText s)]
                -- (toString s) -- (toText s)
runErrorFromEither (Right a) = return a



