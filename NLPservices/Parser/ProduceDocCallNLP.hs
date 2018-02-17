 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceDocCallNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert the whole text to the selection which will be NLP analyzed

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables
    ,TypeSynonymInstances
    , MultiParamTypeClasses
    , NoMonomorphismRestriction
    , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceDocCallNLP
    (module Parser.ProduceDocCallNLP
--    , module CoreNLP.Defs0
--    , module Lines2para.Lines2para
    , module Producer.Servers
--    , module Parser.FilterTextForNLP
    ) where

import              Test.Framework
import              Uniform.TestHarness
import Parser.LanguageTypedText
import Producer.Servers
import CoreNLP.CoreNLPxml (readDocString)
import CoreNLP.Defs0 ()
import Uniform.HttpCallWithConduit (callHTTP10post, addPort2URI, callHTTP8get, addToURI)
import Text.Regex (mkRegex, subRegex)
import Parser.CompleteSentence (completeSentence)
import Parser.ProduceNLPtriples -- (processDoc0toTriples2)
import Parser.LanguageTypedText

import NLP.Corpora.Conll  as Conll -- Conll for english
import NLP.Corpora.ItalianTinT   as TinT-- for italian
import NLP.Corpora.German  as German --
import NLP.Corpora.Spanish as Spanish --
import NLP.Corpora.French as French --
import NLP.Corpora.FrenchUD as FrenchUD --

import Data.Text as T

portGerman = 9001 -- make port type
portEnglish = 9002
portFrench = 9003
portSpanish = 9004
portTinT = 9005
portFrenchUD = 9006


undefConll = undef "convertOneSnip2Triples postag conll":: Conll.POStag
undefGermanPos = undef "convertOneSnip2Triples postag german":: German.POStag
undefTinTPos = undef "convertOneSnip2Triples postat TinT":: TinT.POStag
undefFrenchPos = undef "convertOneSnip2Triples postat French":: French.POStag
undefFrenchUDPos = undef "convertOneSnip2Triples postat FrenchUD":: FrenchUD.POStag
undefSpanishPos = undef "convertOneSnip2Triples postat spanish":: Spanish.POStag

class LanguageDependent lang where

    preNLP :: LTtext lang -> LTtext lang
    -- the processing of the text before NLP
    preNLP = LTtext . cleanTextOther . unLCtext
    nlpPath :: lang -> Text
    nlpPath _ = ""   -- only italian uses a path

instance LanguageDependent EnglishType where
    preNLP    =  LTtext . cleanTextEnglish . unLCtext

instance LanguageDependent GermanType
instance LanguageDependent FrenchType
instance LanguageDependent SpanishType
instance LanguageDependent ItalianType where
    nlpPath _ = "tint"
    preNLP    =  LTtext . cleanTextItalian . unLCtext


class TaggedTyped postag where
    postNLP :: Bool -> URI -> Doc0 postag -> ErrIO (Doc0 postag)
    -- postprocessing (e.g. adding POS to german)
    postNLP _ _ = return

class ( POStags postag, LanguageDependent lang) =>  LanguageTyped2 lang postag where
    snip2doc :: lang -> postag -> Bool ->  LTtext lang -> URI -> ErrIO (Doc0 postag)
    -- the nlp process, selected by language and postag
    snip2doc lph pph debugNLP  text sloc = do
        let debug2 = debugNLP
        docs <-  convertTZ2makeNLPCall pph debug2
                            (addPort2URI sloc (nlpPort lph pph))  -- server uri
                            (nlpPath lph)   -- path
                                (nlpParams lph pph)  (unLCtext text)
        when debug2 $ putIOwords ["NLP end", showT text]
        return docs

    nlpParams :: lang -> postag -> [(Text,Maybe Text)]
    nlpPort :: lang -> postag -> Int   -- should be a port type

class  LanguageTyped22 lang postag where

    convertOneSnip2Triples2 :: lang -> postag -> Bool ->  Snip2 lang -> URI -> ErrIO [NLPtriple postag]
    -- this should be the entry point for conversion of a text to nlp
    -- typed in and output
    -- calls nlp to convert to doc
    -- the snip should have a type parameter language
    -- internal the text2nlp should have a tag type parameter
    -- the triples (i.e. NLPtriples should have a tag parameter

    -- convertOneSnip2Triples2 :: Bool -> Bool -> TextDescriptor -> LTtext lang -> ErrIO [NLPtriple postag]
    -- calls nlp to convert to doc
    -- the snip should have a type parameter language
    -- internal the text2nlp should have a tag type parameter
    -- the triples (i.e. NLPtriples should have a tag parameter

instance (LanguageDependent lang, LanguageTypedText lang, TaggedTyped postag, POStags postag, LanguageTyped2 lang postag)
    =>  LanguageTyped22 lang postag where
    convertOneSnip2Triples2 lph pph debugNLP snip sloc =
        if  snipIsNull snip
            then return []
            else do
                let text = snip2text snip
                when debugNLP $ putIOwords ["convertOneSnip2Triples" -- , sayLanguageOfText text
                                  , "\n text", showT text
                                  , "\n debug", showT debugNLP]
                let text2 = preNLP  text
--                let sloc = nlpServer textstate
                doc1 <- snip2doc lph pph debugNLP   text2 sloc

                doc2 <- postNLP debugNLP  sloc doc1

                let trips = processDoc0toTriples2 lph pph snip doc2

                return trips


instance TaggedTyped Conll.POStag
instance TaggedTyped German.POStag where
    postNLP debug sloc doc1  = do
        let sents1 = docSents doc1
        sents2 <- mapM (completeSentence False (addPort2URI sloc treeTaggerPort ) ) sents1
        let docs2 = doc1{docSents = sents2}
        return docs2

instance TaggedTyped TinT.POStag
instance TaggedTyped Spanish.POStag
instance TaggedTyped French.POStag
instance TaggedTyped FrenchUD.POStag

instance LanguageTyped2 EnglishType Conll.POStag where
    nlpPort _ _ = portEnglish
    nlpParams _ _ =   [("outputFormat", Just "xml")
                , ("annotators", Just "tokenize,ssplit,pos\
                                        \,lemma,ner,depparse,dcoref,coref")]
            --                                    coref -coref.algorithm neural")

instance LanguageTyped2 GermanType German.POStag where
    nlpPort _ _ = portGerman
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]

instance LanguageTyped2 ItalianType TinT.POStag where
    nlpPort _ _ = portTinT
    nlpParams _ _ =   [("format", Just "xml")]

--    nlpParams _ _ =   [("outputFormat", Just "xml"),
--                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
--                                        ]

instance LanguageTyped2 FrenchType French.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

instance LanguageTyped2 FrenchType FrenchUD.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

instance LanguageTyped2 SpanishType Spanish.POStag where
    nlpPort _ _ = portSpanish
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]

class Docs postag where
    convertTZ2makeNLPCall  :: postag -> Bool -> URI -> Text -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger
    -- the path parameter is used for TinT which wants a path "tint" before the paremters


instance (POStags postag) => Docs postag where
--    convertTZ2makeNLPCall  :: Bool ->  URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger
    convertTZ2makeNLPCall ph debugNLP  nlpServer path vars text = do
            when debugNLP $
                putIOwords ["convertTZ2makeNLPCall start"
                            , showT . lengthChar $ text
                            , showT . take' 100 $ text ]
    --        xml ::  Text  <-   makeHttpPost7 debugNLP nlpServer "" vars
    --                    "multipart/form-data" text
            xml :: Text <- callHTTP10post debugNLP "multipart/form-data"  nlpServer path
                     (b2bl . t2b $ text) vars  (Just 300)   -- timeout in seconds
        -- german parser seems to understand utf8encoded bytestring

--            when debugNLP  $
            putIOwords ["convertTZ2makeNLPCall end \n", take' 200 . showT    $  xml]

            doc0 <- readDocString ph True xml                    -- E -> F
                        -- bool controls production of XML output
--            when debugNLP  $
            putIOwords ["convertTZ2makeNLPCall doc0 \n",  take' 200  . showT $ doc0]

            return   doc0
        `catchError` (\e -> do
             putIOwords ["convertTZ2makeNLPCall error caught 7",  e
                            ,  "\n\n the input was \n", text] -- " showT msg])
             putIOwords ["convertTZ2makeNLPCall",  "text:\n",  showT text ] -- " showT msg])
    --         splitAndTryAgain debugNLP showXML nlpServer vars text
             return (Doc0 [] []) -- zero
                )



cleanTextEnglish :: Text -> Text
cleanTextEnglish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words
            . subRegex' "([0-9])([ds])."  "\\1 \\2 "   -- shiling/pence
            . subRegex' "([a-zA-Z]+)-([a-zA-Z]+)" "\\1 \\2"

cleanTextOther :: Text -> Text
cleanTextOther    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextItalian :: Text -> Text
cleanTextItalian    = T.replace "ů" "u" . T.replace "ŕ" "a"     -- missing accents àèéìòóóù
                    . T.replace "č" "e" . T.replace "ň" "o"
                    . T.replace "ě" "i" . T.replace "Č" "e"    -- for perche
                    . T.replace "ę" "a"  -- ?? far provar, Danis??
                    . T.replace "ű" "u" . T.replace "ď" "i"  -- conclusion
                    . subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words
-- Cosě  -- keine akzente gestzt, weil uneinheitlich im italienischen
-- to map would be ŕčňůěČęűď "\341\269\328\367\283\268\281\369\271"
{-
cleanTextGerman :: Text -> Text
cleanTextGerman    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextFrench :: Text -> Text
cleanTextFrench    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextspanish :: Text -> Text
cleanTextspanish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

-}

italianGutenberg = "Cosě, non fu contento finchč. E cosě,   han giŕ voluto che piů vi stesse a dondolare"
italianResult = "Cosi, non fu contento finche. E cosi,   han gia voluto che piu vi stesse a dondolare"

test_italGutenberg = assertEqual italianResult (cleanTextItalian italianGutenberg)

-- Cos\283, non fu contento finche. E cos\283,   han gia voluto che piu vi stesse a dondolare

entz3text = "Test Multiple Languages.   The uncle flew to Boston. When he came into the room, he carried a book. It was red."
gertz3text = "Der Onkel flog nach Boston. Als er den Raum betrat, hatte er ein Buch dabei. Es war rot."
fretz3text = "L'oncle prit l'avion pour Boston. Quand il entrat la chamre, il portait un livre. Il etait rouge."
spantz3text = "El t\237o vol\243 a Boston. Cuando entr\243 en la habitaci\243n, tra\237a un libro. Que era de color rojo."
ittz3text = "Lo zio vol\242 a Boston. Quando entr\242 nella stanza, port\242 un libro. Era rosso."

paraSigl1 =  ParaSigl ( extendSlashRDFsubj "produceDocCallNLP" (RDFsubj $ (showT rdfBase))  )

snip2eng = Snip2 (typeText undefEnglish entz3text) (mkSnipSigl paraSigl1 (SnipID 1))
--nlpserver = serverBrest

testOP_Snip_N (langPh, postagPh, text, i) = do
        putIOwords [ "testOP"]
        let snip  = Snip2 (typeText langPh text) (mkSnipSigl paraSigl1 (SnipID i))
        convertOneSnip2Triples2 langPh postagPh True snip serverBrest

--test_N_1 :: IO ()
test_N_1 = testVar2File (undefEnglish, undefConll, entz3text, 1)        "resultN1" testOP_Snip_N
test_N_2 = testVar2File (undefGerman, undefGermanPos, gertz3text, 2)    "resultN2" testOP_Snip_N
test_N_3 = testVar2File (undefFrench, undefFrenchPos, fretz3text, 3)    "resultN3" testOP_Snip_N
test_N_4 = testVar2File (undefSpanish, undefSpanishPos, spantz3text, 4) "resultN4" testOP_Snip_N
test_N_5 = testVar2File (undefItalian, undefTinTPos, ittz3text, 5)      "resultN5" testOP_Snip_N


--------------------------


subRegex' :: Text -> Text -> Text -> Text
-- replace the in the t the regex with the replacement
subRegex' reg rep t = s2t $ subRegex (mkRegex . t2s $ reg) (t2s t) (t2s rep)

test_clean1 = assertEqual "The father went to the row house for 2 d  and got it."
        (cleanTextEnglish "The _father_ went to the row-house for 2d. and got it.")
test_clean2 = assertEqual "  Knots of idle men  on the South Bridge, for 3 s  2 d  .\
        \   This street named the Via Dolorosa."
 (cleanTextEnglish tx1)

tx1 = "  Knots of idle-men  \
    \on the South Bridge, for 3s. 2d. .   \
    \This street named the _Via Dolorosa_."

