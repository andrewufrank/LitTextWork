-----------------------------------------------------------------------------
--
-- Module      :  complete the sentence in Defs0 mit lemma and a second PoS
-- Copyright   : af
--
-- conversin F -> G
-- is calling sentence by sentence for german lemmatization
-- if other lemmatization are necessary, then select different port

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables
--        , BangPatterns
            , UndecidableInstances
         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Parser.CompleteSentence (
    module Parser.CompleteSentence
    -- completeSentence
--    , htf_thisModulesTests
    , module Producer.Servers
    )   where

--import           Test.Framework
import Uniform.Error
import Producer.Servers
import Parser.ConvertTaggerOutput--import NLP.CallTagger2
import CoreNLP.Defs0
import NLP.Corpora.Conll
--import BuchCode.BuchToken (LanguageCode(..))

import Uniform.HttpCallWithConduit

class ExtractSentences postag where
    extractTokens :: Sentence0 postag -> [Text]

    putTags2token :: TTdata -> Token0 postag -> Token0 postag
-- insert the tag value to the token

    completeSentence :: Bool -> URI ->   (Sentence0 postag) -> ErrIO (Sentence0 postag)

instance (Show postag) => ExtractSentences postag where
    putTags2token tt tok =  if (word0 . tword $ tok) == ttwf tt
                then tlemma' (const . Lemma0 . ttlemma $ tt) . tpostt' (const . ttpos $ tt) $ tok
                else errorT ["putTags2token", "not the same wordform", word0 . tword $ tok, "tagger gives", ttwf tt]

    extractTokens  =   map (word0 . tword) . stoks

    completeSentence debugCS server   sent1 = do
        when debugCS $ putIOwords ["completeSentence start", showT sent1]
        let  toks = extractTokens sent1  -- not working: made strict in text to delay till text is available
                    -- may resolve problem of error in accept (limit 5 caller)
        ttres <- ttProcess server   toks   -- replace by httpcall
        let tags = convertTT ttres
        let toks2 = zipWith putTags2token tags (stoks sent1)
        let sent2 = sent1{stoks = toks2}

        let sent5 = sent2
        when debugCS $ putIOwords ["completeSentence end", showT sent5]
        return sent5


ttProcess :: URI -> [Text] ->ErrIO Text
-- just the call to the server at 17701 ttscottyServer
ttProcess server toks  = callHTTP10post False "text/plain" server ""
                (b2bl . t2b . unlines' $ toks) [] Nothing

tlemma' f t = t{tlemma = f . tlemma $ t}
tpostt' f t = t{tpostt = f . tpostt $ t}



