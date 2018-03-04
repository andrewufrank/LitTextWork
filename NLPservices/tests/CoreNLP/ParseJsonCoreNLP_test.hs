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
module CoreNLP.ParseJsonCoreNLP_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (eitherDecode)

import CoreNLP.ParseJsonCoreNLP

test_nlpjson  = do
    res0 <- runErr $ do
        let fn = makeRelFile "short1.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
--        putIOwords ["json input:",showStartJson f]
        let r = eitherDecode  f  :: Either String Doc2
--        putIOwords ["decoded:", showT r]
        runErrorFromEither r
--        return r
    assertEqual restest_nlpjson  (show res0)

restest_nlpjson = "Right (Doc2 {doc_sentences = [Sentence2 {s_index = 0, s_parse = \"(ROOT\\n  (S\\n    (SBAR\\n      (WHADVP (WRB When))\\n      (S\\n        (NP (DT the) (NN uncle))\\n        (VP (VBD came)\\n          (PP (IN into)\\n            (NP (DT the) (NN room))))))\\n    (, ,)\\n    (NP (PRP he))\\n    (VP (VBD carried)\\n      (NP (DT a) (NN book)))\\n    (. .)))\", s_basicDependencies = Just [Dependency2 {dep_dep = \"ROOT\", dep_governor = 0, dep_governorGloss = \"ROOT\", dep_dependent = 10, dep_dependentGloss = \"carried\"},Dependency2 {dep_dep = \"advmod\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 1, dep_dependentGloss = \"When\"},Dependency2 {dep_dep = \"det\", dep_governor = 3, dep_governorGloss = \"uncle\", dep_dependent = 2, dep_dependentGloss = \"the\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 3, dep_dependentGloss = \"uncle\"},Dependency2 {dep_dep = \"advcl\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 4, dep_dependentGloss = \"came\"},Dependency2 {dep_dep = \"case\", dep_governor = 7, dep_governorGloss = \"room\", dep_dependent = 5, dep_dependentGloss = \"into\"},Dependency2 {dep_dep = \"det\", dep_governor = 7, dep_governorGloss = \"room\", dep_dependent = 6, dep_dependentGloss = \"the\"},Dependency2 {dep_dep = \"nmod\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 7, dep_dependentGloss = \"room\"},Dependency2 {dep_dep = \"punct\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 8, dep_dependentGloss = \",\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 9, dep_dependentGloss = \"he\"},Dependency2 {dep_dep = \"det\", dep_governor = 12, dep_governorGloss = \"book\", dep_dependent = 11, dep_dependentGloss = \"a\"},Dependency2 {dep_dep = \"dobj\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 12, dep_dependentGloss = \"book\"},Dependency2 {dep_dep = \"punct\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 13, dep_dependentGloss = \".\"}], s_enhancedDependencies = Just [Dependency2 {dep_dep = \"ROOT\", dep_governor = 0, dep_governorGloss = \"ROOT\", dep_dependent = 10, dep_dependentGloss = \"carried\"},Dependency2 {dep_dep = \"advmod\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 1, dep_dependentGloss = \"When\"},Dependency2 {dep_dep = \"det\", dep_governor = 3, dep_governorGloss = \"uncle\", dep_dependent = 2, dep_dependentGloss = \"the\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 3, dep_dependentGloss = \"uncle\"},Dependency2 {dep_dep = \"advcl\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 4, dep_dependentGloss = \"came\"},Dependency2 {dep_dep = \"case\", dep_governor = 7, dep_governorGloss = \"room\", dep_dependent = 5, dep_dependentGloss = \"into\"},Dependency2 {dep_dep = \"det\", dep_governor = 7, dep_governorGloss = \"room\", dep_dependent = 6, dep_dependentGloss = \"the\"},Dependency2 {dep_dep = \"nmod:into\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 7, dep_dependentGloss = \"room\"},Dependency2 {dep_dep = \"punct\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 8, dep_dependentGloss = \",\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 9, dep_dependentGloss = \"he\"},Dependency2 {dep_dep = \"det\", dep_governor = 12, dep_governorGloss = \"book\", dep_dependent = 11, dep_dependentGloss = \"a\"},Dependency2 {dep_dep = \"dobj\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 12, dep_dependentGloss = \"book\"},Dependency2 {dep_dep = \"punct\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 13, dep_dependentGloss = \".\"}], s_enhancedPlusPlusDependencies = Just [Dependency2 {dep_dep = \"ROOT\", dep_governor = 0, dep_governorGloss = \"ROOT\", dep_dependent = 10, dep_dependentGloss = \"carried\"},Dependency2 {dep_dep = \"advmod\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 1, dep_dependentGloss = \"When\"},Dependency2 {dep_dep = \"det\", dep_governor = 3, dep_governorGloss = \"uncle\", dep_dependent = 2, dep_dependentGloss = \"the\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 3, dep_dependentGloss = \"uncle\"},Dependency2 {dep_dep = \"advcl\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 4, dep_dependentGloss = \"came\"},Dependency2 {dep_dep = \"case\", dep_governor = 7, dep_governorGloss = \"room\", dep_dependent = 5, dep_dependentGloss = \"into\"},Dependency2 {dep_dep = \"det\", dep_governor = 7, dep_governorGloss = \"room\", dep_dependent = 6, dep_dependentGloss = \"the\"},Dependency2 {dep_dep = \"nmod:into\", dep_governor = 4, dep_governorGloss = \"came\", dep_dependent = 7, dep_dependentGloss = \"room\"},Dependency2 {dep_dep = \"punct\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 8, dep_dependentGloss = \",\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 9, dep_dependentGloss = \"he\"},Dependency2 {dep_dep = \"det\", dep_governor = 12, dep_governorGloss = \"book\", dep_dependent = 11, dep_dependentGloss = \"a\"},Dependency2 {dep_dep = \"dobj\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 12, dep_dependentGloss = \"book\"},Dependency2 {dep_dep = \"punct\", dep_governor = 10, dep_governorGloss = \"carried\", dep_dependent = 13, dep_dependentGloss = \".\"}], s_entitymentions = [Ner2 {ner_docTokenBegin = 8, ner_docTokenEnd = 9, ner_tokenBegin = 8, ner_tokenEnd = 9, ner_text = \"he\", ner_characterOffsetBegin = 35, ner_characterOffsetEnd = 37, ner_ner = \"PERSON\"}], s_tokens = [Token2 {tok_index = 1, tok_word = \"When\", tok_originalText = \"When\", tok_lemma = \"when\", tok_characterOffsetBegin = 0, tok_characterOffsetEnd = 4, tok_pos = \"WRB\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \"\", tok_after = \" \"},Token2 {tok_index = 2, tok_word = \"the\", tok_originalText = \"the\", tok_lemma = \"the\", tok_characterOffsetBegin = 5, tok_characterOffsetEnd = 8, tok_pos = \"DT\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 3, tok_word = \"uncle\", tok_originalText = \"uncle\", tok_lemma = \"uncle\", tok_characterOffsetBegin = 9, tok_characterOffsetEnd = 14, tok_pos = \"NN\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 4, tok_word = \"came\", tok_originalText = \"came\", tok_lemma = \"come\", tok_characterOffsetBegin = 15, tok_characterOffsetEnd = 19, tok_pos = \"VBD\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 5, tok_word = \"into\", tok_originalText = \"into\", tok_lemma = \"into\", tok_characterOffsetBegin = 20, tok_characterOffsetEnd = 24, tok_pos = \"IN\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 6, tok_word = \"the\", tok_originalText = \"the\", tok_lemma = \"the\", tok_characterOffsetBegin = 25, tok_characterOffsetEnd = 28, tok_pos = \"DT\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 7, tok_word = \"room\", tok_originalText = \"room\", tok_lemma = \"room\", tok_characterOffsetBegin = 29, tok_characterOffsetEnd = 33, tok_pos = \"NN\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \"\"},Token2 {tok_index = 8, tok_word = \",\", tok_originalText = \",\", tok_lemma = \",\", tok_characterOffsetBegin = 33, tok_characterOffsetEnd = 34, tok_pos = \",\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \"\", tok_after = \" \"},Token2 {tok_index = 9, tok_word = \"he\", tok_originalText = \"he\", tok_lemma = \"he\", tok_characterOffsetBegin = 35, tok_characterOffsetEnd = 37, tok_pos = \"PRP\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 10, tok_word = \"carried\", tok_originalText = \"carried\", tok_lemma = \"carry\", tok_characterOffsetBegin = 38, tok_characterOffsetEnd = 45, tok_pos = \"VBD\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 11, tok_word = \"a\", tok_originalText = \"a\", tok_lemma = \"a\", tok_characterOffsetBegin = 46, tok_characterOffsetEnd = 47, tok_pos = \"DT\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 12, tok_word = \"book\", tok_originalText = \"book\", tok_lemma = \"book\", tok_characterOffsetBegin = 48, tok_characterOffsetEnd = 52, tok_pos = \"NN\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \"\"},Token2 {tok_index = 13, tok_word = \".\", tok_originalText = \".\", tok_lemma = \".\", tok_characterOffsetBegin = 52, tok_characterOffsetEnd = 53, tok_pos = \".\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \"\", tok_after = \" \"}]},Sentence2 {s_index = 1, s_parse = \"(ROOT\\n  (S\\n    (NP (PRP It))\\n    (VP (VBD was)\\n      (ADJP (JJ red)))\\n    (. .)))\", s_basicDependencies = Just [Dependency2 {dep_dep = \"ROOT\", dep_governor = 0, dep_governorGloss = \"ROOT\", dep_dependent = 3, dep_dependentGloss = \"red\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 1, dep_dependentGloss = \"It\"},Dependency2 {dep_dep = \"cop\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 2, dep_dependentGloss = \"was\"},Dependency2 {dep_dep = \"punct\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 4, dep_dependentGloss = \".\"}], s_enhancedDependencies = Just [Dependency2 {dep_dep = \"ROOT\", dep_governor = 0, dep_governorGloss = \"ROOT\", dep_dependent = 3, dep_dependentGloss = \"red\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 1, dep_dependentGloss = \"It\"},Dependency2 {dep_dep = \"cop\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 2, dep_dependentGloss = \"was\"},Dependency2 {dep_dep = \"punct\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 4, dep_dependentGloss = \".\"}], s_enhancedPlusPlusDependencies = Just [Dependency2 {dep_dep = \"ROOT\", dep_governor = 0, dep_governorGloss = \"ROOT\", dep_dependent = 3, dep_dependentGloss = \"red\"},Dependency2 {dep_dep = \"nsubj\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 1, dep_dependentGloss = \"It\"},Dependency2 {dep_dep = \"cop\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 2, dep_dependentGloss = \"was\"},Dependency2 {dep_dep = \"punct\", dep_governor = 3, dep_governorGloss = \"red\", dep_dependent = 4, dep_dependentGloss = \".\"}], s_entitymentions = [], s_tokens = [Token2 {tok_index = 1, tok_word = \"It\", tok_originalText = \"It\", tok_lemma = \"it\", tok_characterOffsetBegin = 54, tok_characterOffsetEnd = 56, tok_pos = \"PRP\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 2, tok_word = \"was\", tok_originalText = \"was\", tok_lemma = \"be\", tok_characterOffsetBegin = 57, tok_characterOffsetEnd = 60, tok_pos = \"VBD\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \" \"},Token2 {tok_index = 3, tok_word = \"red\", tok_originalText = \"red\", tok_lemma = \"red\", tok_characterOffsetBegin = 61, tok_characterOffsetEnd = 64, tok_pos = \"JJ\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \" \", tok_after = \"\"},Token2 {tok_index = 4, tok_word = \".\", tok_originalText = \".\", tok_lemma = \".\", tok_characterOffsetBegin = 64, tok_characterOffsetEnd = 65, tok_pos = \".\", tok_ner = \"O\", tok_speaker = \"PER0\", tok_before = \"\", tok_after = \"\"}]}], doc_corefs = Coreferences2 {chains = [CorefChain2 [Coref2 {coref_id = 1, coref_text = \"the uncle\", coref_startIndex = 2, coref_endIndex = 4, coref_headIndex = 3, coref_sentNum = 1, coref_isRepresentativeMention = True},Coref2 {coref_id = 3, coref_text = \"he\", coref_startIndex = 9, coref_endIndex = 10, coref_headIndex = 9, coref_sentNum = 1, coref_isRepresentativeMention = False}],CorefChain2 [Coref2 {coref_id = 4, coref_text = \"a book\", coref_startIndex = 11, coref_endIndex = 13, coref_headIndex = 12, coref_sentNum = 1, coref_isRepresentativeMention = True}],CorefChain2 [Coref2 {coref_id = 2, coref_text = \"the room\", coref_startIndex = 6, coref_endIndex = 8, coref_headIndex = 7, coref_sentNum = 1, coref_isRepresentativeMention = True},Coref2 {coref_id = 5, coref_text = \"It\", coref_startIndex = 1, coref_endIndex = 2, coref_headIndex = 1, coref_sentNum = 2, coref_isRepresentativeMention = False}]]}})"



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

