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
module Lib.ProduceNLPtriples2_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (eitherDecode)

import Lib.ParseJsonCoreNLP
import Lib.Doc2ToDoc0
import Lib.ProduceNLPtriples2
import qualified NLP.Corpora.Conll  as Conll
--import Text.Show.Pretty (valToStr)
--import Text.PrettyPrint.GenericPretty
import Parser.LanguageTypedText
import Parser.NLPvocabulary (ParaSigl (..), SnipID (..), mkSnipSigl)
import Data.RDF.Extension
import Producer.Servers

test_triples1  = do
    res0 <- runErr $ do
        let fn = makeRelFile "short1.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
--        putIOwords ["json input:",showStartJson f]
        let r = eitherDecode  f  :: Either String Doc2
--        let doc2 :: Doc2
        doc2 :: Doc2 <- case r of
                Left msg ->  throwErrorT ["doc2 left",  s2t msg]
                Right a -> return a
        putIOwords ["doc2:", take' 200 $ showT doc2]
        let doc1 = doc2to1 Conll.undefConll doc2
        putIOwords ["doc1:", take' 200 $ showT doc1]
        let trip = processDoc1toTriples2 undefEnglish Conll.undefConll
                sigl1 doc1
        putIOwords ["trips", showT $ take 20 trip]
        return trip
    assertEqual restest_nlpjson2  (show res0)

restest_nlpjson2 = ""
--
paraSigl1 =  ParaSigl ( extendSlashRDFsubj "produceDocCallNLP"
        (RDFsubj $ (unPartURI rdfBase))  )
sigl1 = mkSnipSigl paraSigl1 (SnipID 1)

--snip2eng = Snip2 (typeText undefEnglish entz3text) (mkSnipSigl paraSigl1 (SnipID 1))
--nlpserver = serverBrest


showStartJson = s2t . take 100 . B.toString

runErrorFromEither :: (Show s, CharChains s) => Either s a -> ErrIO a
runErrorFromEither (Left s) = throwErrorT ["runErrorFromEither", (toText s)]
                -- (toString s) -- (toText s)
runErrorFromEither (Right a) = return a

---- show produces the "xx"
--test_1 = do
--    res0 <- runErr $ do
--        let fn = makeRelFile "short1.json"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:", take' 100 . showT $ f]
--        let r = decodeDoc1 f  :: Either String [Doc1]
--        putIOwords ["decoded:", showT r]
--        return r
--    assertEqual res (show res0)
--
--res =  ""


