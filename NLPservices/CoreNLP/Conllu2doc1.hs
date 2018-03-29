-----------------------------------------------------------------------------
--
-- Module      :  the top exporting all contents of CoreNLP

-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
        , FlexibleInstances
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
    , RecordWildCards
    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CoreNLP.Conllu2doc1 (
        module CoreNLP.Conllu2doc1
        )  where

import Conllu.Print as Pr
import Conllu.Type as T
import qualified Conllu.Parse as P -- (document, Parser)
            -- this is the parser!
import qualified Text.Megaparsec as M

import CoreNLP.DocNLP_0or1 -- (ConvertTo1 (..), LanguageCode (..))
import qualified NLP.TagSets.UD as UD
import qualified NLP.Tags as NLP

--json2NT :: PartURI -> Text -> NTtext
---- | main operation, convert JSON text file to a triple (NT) text file
--json2NT rdfbase = toNT . toTriple rdfbase . toLin . to11op . to1op .  decodeDoc2op
--
--conllu2NT rdfbase = toNT . toTriple rdfbase . toLin . to11op . conllu2doc1

conllu2doc1 :: Text -> Doc1 UD.POStag
conllu2doc1  t = convertTo1 (UD.undefPOS) English  ss2 -- (fromRightEOV ss1)
    where
        ss1 = parseConllu (P.documentC P.sentence) $ t :: ErrOrVal [T.Sentence]
        ss2 = case ss1 of
--            Right ss -> errorT ["result conllu2doc1", showT ss] :: [T.Sentence]
                            -- unlines' $ ma
--                            showT ss
            Right ss -> ss :: [T.Sentence]
            Left msg -> errorT ["conllu2doc1 parse error: ", msg]

prettyPrintConlluSentence ::T.Sentence -> Text
-- | prettyprint a single sentence
prettyPrintConlluSentence s = s2t . Pr.fromDiffList . Pr.printSent $ s

-- utilities
parseConllu :: P.Parser [T.Sentence] -> Text -> ErrOrVal [T.Sentence]
-- | parse a text (no IO)
parseConllu parser text =
    case r of
            Left err -> Left (s2t $ M.parseErrorPretty err)
            Right ss -> Right   ss
    where
        r = M.parse parser "" (t2s text)   -- why a textname required ??

-- convert to Doc1
instance  ConvertTo1 UD.POStag  [T.Sentence] (Doc1 UD.POStag) where
    convertTo1 postag lang sents = Doc1 {..}
        where
            doc1sents = zipWith  (convertTo1sentence  postag lang) [1..] sents
            -- needs numbering of sentences
            doc1corefs = Nothing

convertTo1sentence :: UD.POStag -> LanguageCode -> Int -> T.Sentence -> Sentence1 UD.POStag
--instance ConvertTo1 postag  T.Sentence (Sentence1 postag) where
convertTo1sentence  postag lang i T.Sentence {..} = Sentence1 {..}
        where
            s1id = SentenceID i  -- must start with 1 to conform with coreNLP in general
            s1parse = Nothing
            s1toks = map (convertTo1 postag lang) _tokens
            s1deps = Just $ map (convertTo1deps postag lang) _tokens
                    -- extract the dependencies from the tokens
            s1entitymentions = Nothing

instance  ConvertTo1 UD.POStag  T.Token (Token0 UD.POStag) where
    convertTo1 _ lang T.SToken {..} = Token0 {..}
      where
        tid = TokenID  _ix
        tword = maybe zero   (\l -> Wordform0 $ LCtext  (s2t l) lang) _form
        twordOrig = Nothing
--        if tok_word == tok_originalText then Nothing else Just tok_originalText
        tlemma = maybe zero (\l -> Lemma0 $ LCtext (s2t l) lang) _lemma
        tpos = maybe UD.unkPOStag (\p -> ( NLP.toPOStag  . showT $ p ) )   _upostag
        tfeature = _feats
        tposOrig = Just . showT $ _upostag
--        if showT pos == tok_pos then Nothing else Just tok_pos
        -- missig a test that parse was complete
--        tpostt = Nothing
        tner = []
--        parseNERtagList [tok_ner] -- when is this a list?
        tnerOrig = Nothing
--            if (any isAnUnknownNER $ tner) then Just [tok_ner] else Nothing
                        -- use the Ner2 values?
        tspeaker = []
--            parseSpeakerTagList . maybeToList $ tok_speaker
--                    maybe [] (\a -> [a]) $ tok_speaker
        tbegin = zero -- tok_characterOffsetBegin
        tend = zero --  tok_characterOffsetEnd
        tbefore = Just $ if tpos == UD.PUNCT then "" else " " -- tok_before
        tafter = Just "" -- tok_after

convertTo1deps :: postag -> LanguageCode -> T.Token -> Dependence1
convertTo1deps _ lang T.SToken {..} = Dependence1 {..}
        where
            d1type = maybe (unkDEPtag) (id)  _deprel  :: DepCode
            d1orig = zero -- dep_dep
            d1govid = maybe zero TokenID _dephead
            d1depid = TokenID _ix
            d1govGloss = zero -- LCtext {ltxt = dep_governorGloss, llang = lang}
            d1depGloss = maybe zero   (\l ->  LCtext  (s2t l) lang) _form

