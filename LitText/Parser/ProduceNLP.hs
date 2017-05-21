-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | consist of two parts - preparation of the data to submit and
-- analysis of the return
--
-- version 2 assumes that each paragraph is individually analyzed -
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLP
    (module Parser.ProduceNLP
--    (convertTZ2nlp
--    , prepareTZ4nlp
--    , ErrIO (..)
--    , htf_thisModulesTests
--    , result1E_nlpResult
    ) where

import           Test.Framework

--import           CoreNLP.Snippets2nt          (makeNLPrequest5) -- , readDocString)
import           Data.RDF
-- import           Data.RDF.Extension
--import           Data.Text.Encoding           (decodeLatin1, encodeUtf8)
import           Parser.Foundation  -- todo should be comming up
import Lines2para.Lines2para
import Lines2para.HandleLayout
--import           Lines2para.Lines2paraTests
----    (result1C_tzResult1, result1A_textstate)  -- for testing
import Parser.ReadMarkupAB  -- todo

--import           Parser.ProduceLit            (buchURIx, paraSigl)
--import           Uniform.Convenience.StartApp
--import           Uniform.FileIO
--import           Uniform.Strings              hiding ((<|>))
import Uniform.Error  -- todo should be comming up
import Uniform.HttpGet
--import Parser.ProduceNLPtriples

debugNLP = False

data NLPtext = NLPtext { tz3loc :: TextLoc
                    , tz3text:: Text
                    , tz3lang :: LanguageCode }
            deriving (Show, Eq )

prepareTZ4nlp :: [TZ2] -> [NLPtext]  -- test C  -> D
-- selecte the text from TZ and convert to text
prepareTZ4nlp = map formatParaText . filter condNLPtext
        ---------------------------------preparing for analysis

condNLPtext :: TZ2 -> Bool
-- select the paragraphs and the titles to TZtext
condNLPtext tz  = case tz of
--    TZzdahl {}  -> errorT ["condNLPtext","should not have TZzahl left", showT tz]
    TZ2markup {} ->
            case tz2tok tz of
                BuchTitel ->  True
                BuchHL1   ->  True
                BuchHL2   ->  True
                BuchHL3   ->  True
                _         ->   False
--                    buchEnde ... errorT ["ProduceLit.hs conv2", "missing Markup case", showT . tztok $ tz]
--    TZleer {} -> False  -- errorT ["condNLPtext","should not haveleer", showT tz]
--    TZtext {} -> errorT ["condNLPtext","should not have single text", showT tz]
--    TZkurz {} -> errorT ["condNLPtext","should not have single kurz", showT tz]
    TZ2para {} -> True

--    TZkurz {} -> p : condNLPtext rest
--                        where (p,rest) = collectKurz (t:ts)
--    _ -> errorT ["ProduceLit.hs conv2", "missing TZ case", showT tz]

formatParaText :: TZ2 -> NLPtext
-- convert the headers to a tztext
formatParaText tz@TZ2para{} = NLPtext {tz3loc = tz2loc tz, tz3lang = tz2lang tz
        , tz3text = foldl1 combine2linesWithHyphenation
            . map (twm . tztext) $ (tz2tzs tz)
        }

formatParaText tz@TZ2markup {} = NLPtext {tz3loc = tz2loc tz
        , tz3lang = tz2lang tz
        , tz3text =  twm . tz2text $ tz}

--serverLoc = localhost
--serverbrest = "nlp.gerastree.at"
--localhost = "127.0.0.1"

nlpServerEnglish, nlpServerGerman, nlpServerNone ::PartURI -> PartURI
nlpServerEnglish loc =   loc <> ":9000"  -- not localhost!
--nlpServer = "http://nlp.gerastree.at:9000"
nlpServerGerman loc =   loc <> ":9001"  -- for german
nlpServerNone loc = nlpServerEnglish loc
-- for no language which can be processed
-- should be a server just returning the input tokenized etc

----------------
test_prepareTZ4nlp :: IO ()  -- C -> D
test_prepareTZ4nlp =  do
    putIOwords ["prepareTZ4nlp:   "] -- tzResult]
    let t1 = prepareTZ4nlp result1BAE
--    putIOwords ["prepareTZ4nlp: result (for next) ", s2t $ show t1]
--    putIOwords ["prepareTZ4nlp:  result ", show' t1]
    assertEqual result1D t1

-------------------------------------------------D -> E

convertTZ2nlp :: PartURI -> NLPtext -> ErrIO (NLPtext,Text)   -- the xml to analyzse  D -> E
-- send a tz text to coreNLP server
-- works on individual paragraphs
convertTZ2nlp sloc tz = do
    when debugNLP $ putIOwords ["convertTZ2nlp"]

    let language = tz3lang tz
    let text = tz3text tz

    let nlpServer = case language of
                    English -> nlpServerEnglish sloc
                    German -> nlpServerGerman sloc
                    NoLanguage -> nlpServerNone sloc
                    _ -> errorT ["convertTZ2nlp", showT language, "language has no server"]

--        let textstate2 = textstate {language = language}
--    let requestBytestring = case language of   -- this should be captured in the server
--                    English -> text
--                    German ->  (decodeLatin1 . encodeUtf8 ) text
--                    NoLanguage -> text
--                    _ -> errorT ["convertTZ2nlp", showT language, "language has no conversion"]
--                    -- the german parser requires latin1 !!

--    lbs ::  Text  <- do     ret <- makeNLPrequest6 True nlpServer (t2b text)
--                            return $  bb2t ret
--    lbs ::  Text  <-   makeNLPrequest5 False nlpServer text
    let vars =  [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--                    -- removed ,coref
                    , ("outputFormat","xml")
                    ]
    lbs ::  Text  <-   makeHttpPost7 False nlpServer vars "text/plain" text

--    lbs ::  Text  <- case language of
--                    English -> do
--                                    ret <- makeNLPrequest6 True nlpServer (t2b text)
--                                    return $  bb2t ret
--                    German ->  do
--                                    ret <- makeNLPrequest6 True nlpServer (t2b text) -- (t3latin text)
--                                    return $ bb2t ret -- latin2t ret
--
--                    NoLanguage -> do
--                                    ret <- makeNLPrequest6 True nlpServer (t2b text)
--                                    return $  bb2t ret
-- german parser seems to understand utf8encoded bytestring

--    makeNLPrequest nlpServer (text2)  -- should be bytestring
--    -- try twice
--    lbs <- case mlbs of
--        Nothing -> do
--                putIOwords ["convertTZ2nlp first request not successfull"]
--                errorT ["request to nlp server was not successful, quit"]
--                mlbs2 <- makeNLPrequest nlpServer (text2)
--                case mlbs2 of
--                    Nothing -> do
--                            putIOwords ["convertTZ2nlp second request not successfull"]
--                            throwErrorT ["convertTZ2nlp second request not successfull"]
--                    Just lbs -> return lbs
--        Just lbs ->
    return lbs

    when debugNLP  $ putIOwords ["convertTZ2nlp end \n", showT lbs]

    return (tz,lbs)


test_1_D_E_convertTZ2nlp ::   IO ()  -- D -> E
test_1_D_E_convertTZ2nlp =  do
    putIOwords ["convertTZ2nlp: result1D to result1E  "] -- tzResult]
    let sloc = serverLoc result1A
    res <- mapM (runErr . convertTZ2nlp sloc) result1D
--    putIOwords ["prepareTZ4nlp: result (for next) ", s2t $ show t1]
--    putIOwords ["prepareTZ4nlp:  result ", show' t1]
    assertEqual result1E res

result1E =
    [Right
       (NLPtext{tz3loc = TextLoc{tlpage = "11", tlline = 6},
                tz3text = "(Krieg f\252r Welt)", tz3lang = German},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>-LRB-</word><lemma>-lrb-</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>1</CharacterOffsetEnd><POS>TRUNC</POS><NER>O</NER></token><token id=\"2\"><word>Krieg</word><lemma>krieg</lemma><CharacterOffsetBegin>1</CharacterOffsetBegin><CharacterOffsetEnd>6</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"3\"><word>f\252r</word><lemma>f\252r</lemma><CharacterOffsetBegin>7</CharacterOffsetBegin><CharacterOffsetEnd>10</CharacterOffsetEnd><POS>APPR</POS><NER>O</NER></token><token id=\"4\"><word>Welt</word><lemma>welt</lemma><CharacterOffsetBegin>11</CharacterOffsetBegin><CharacterOffsetEnd>15</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"5\"><word>-RRB-</word><lemma>-rrb-</lemma><CharacterOffsetBegin>15</CharacterOffsetBegin><CharacterOffsetEnd>16</CharacterOffsetEnd><POS>TRUNC</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (NUR\n    (S\n      (NP\n        (CNP (TRUNC -LRB-) (NN Krieg))\n        (PP (APPR f\252r) (NN Welt)))\n      (VP\n        (CVP\n          (VP (TRUNC -RRB-)))))))\n\n</parse></sentence></sentences></document></root>\r\n"),
     Right
       (NLPtext{tz3loc = TextLoc{tlpage = "12", tlline = 8},
                tz3text = "Unsere Br\228uche werden lebendig", tz3lang = German},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>Unsere</word><lemma>unsere</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>6</CharacterOffsetEnd><POS>PPOSAT</POS><NER>O</NER></token><token id=\"2\"><word>Br\228uche</word><lemma>br\228uche</lemma><CharacterOffsetBegin>7</CharacterOffsetBegin><CharacterOffsetEnd>14</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"3\"><word>werden</word><lemma>werden</lemma><CharacterOffsetBegin>15</CharacterOffsetBegin><CharacterOffsetEnd>21</CharacterOffsetEnd><POS>VAFIN</POS><NER>O</NER></token><token id=\"4\"><word>lebendig</word><lemma>lebendig</lemma><CharacterOffsetBegin>22</CharacterOffsetBegin><CharacterOffsetEnd>30</CharacterOffsetEnd><POS>ADJD</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (NUR\n    (S\n      (NP (PPOSAT Unsere) (NN Br\228uche))\n      (VAFIN werden) (ADJD lebendig))))\n\n</parse></sentence></sentences></document></root>\r\n"),
     Right
       (NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 10},
                tz3text =
                  "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
                tz3lang = German},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>Was</word><lemma>was</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>3</CharacterOffsetEnd><POS>PWS</POS><NER>O</NER></token><token id=\"2\"><word>w\252rde</word><lemma>w\252rde</lemma><CharacterOffsetBegin>4</CharacterOffsetBegin><CharacterOffsetEnd>9</CharacterOffsetEnd><POS>VAFIN</POS><NER>O</NER></token><token id=\"3\"><word>ihm</word><lemma>ihm</lemma><CharacterOffsetBegin>10</CharacterOffsetBegin><CharacterOffsetEnd>13</CharacterOffsetEnd><POS>PPER</POS><NER>O</NER></token><token id=\"4\"><word>fremd</word><lemma>fremd</lemma><CharacterOffsetBegin>14</CharacterOffsetBegin><CharacterOffsetEnd>19</CharacterOffsetEnd><POS>ADJD</POS><NER>O</NER></token><token id=\"5\"><word>und</word><lemma>und</lemma><CharacterOffsetBegin>20</CharacterOffsetBegin><CharacterOffsetEnd>23</CharacterOffsetEnd><POS>KON</POS><NER>O</NER></token><token id=\"6\"><word>was</word><lemma>was</lemma><CharacterOffsetBegin>24</CharacterOffsetBegin><CharacterOffsetEnd>27</CharacterOffsetEnd><POS>PWS</POS><NER>O</NER></token><token id=\"7\"><word>m\246chte</word><lemma>m\246chte</lemma><CharacterOffsetBegin>28</CharacterOffsetBegin><CharacterOffsetEnd>34</CharacterOffsetEnd><POS>VMFIN</POS><NER>O</NER></token><token id=\"8\"><word>sein</word><lemma>sein</lemma><CharacterOffsetBegin>35</CharacterOffsetBegin><CharacterOffsetEnd>39</CharacterOffsetEnd><POS>PPOSAT</POS><NER>O</NER></token><token id=\"9\"><word>eigen</word><lemma>eigen</lemma><CharacterOffsetBegin>40</CharacterOffsetBegin><CharacterOffsetEnd>45</CharacterOffsetEnd><POS>ADJD</POS><NER>O</NER></token><token id=\"10\"><word>sein</word><lemma>sein</lemma><CharacterOffsetBegin>46</CharacterOffsetBegin><CharacterOffsetEnd>50</CharacterOffsetEnd><POS>VAINF</POS><NER>O</NER></token><token id=\"11\"><word>in</word><lemma>in</lemma><CharacterOffsetBegin>51</CharacterOffsetBegin><CharacterOffsetEnd>53</CharacterOffsetEnd><POS>APPR</POS><NER>O</NER></token><token id=\"12\"><word>C\233rb\232re</word><lemma>c\233rb\232re</lemma><CharacterOffsetBegin>54</CharacterOffsetBegin><CharacterOffsetEnd>61</CharacterOffsetEnd><POS>NN</POS><NER>I-LOC</NER></token><token id=\"13\"><word>?</word><lemma>?</lemma><CharacterOffsetBegin>61</CharacterOffsetBegin><CharacterOffsetEnd>62</CharacterOffsetEnd><POS>$.</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (CS\n    (S\n      (NP (PWS Was))\n      (VAFIN w\252rde) (PPER ihm) (ADJD fremd))\n    (KON und)\n    (S (PWS was) (VMFIN m\246chte)\n      (VP\n        (NP\n          (CNP\n            (NP (PPOSAT sein)\n              (CNP\n                (NP\n                  (AP (ADJD eigen)))))))\n        (VAINF sein)\n        (PP (APPR in) (NN C\233rb\232re))))\n    ($. ?)))\n\n</parse></sentence></sentences></document></root>\r\n"),
     Right
       (NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 12},
                tz3text = "Er fragte sich als zweiter Paragraph.",
                tz3lang = German},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>Er</word><lemma>er</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>2</CharacterOffsetEnd><POS>PPER</POS><NER>O</NER></token><token id=\"2\"><word>fragte</word><lemma>fragte</lemma><CharacterOffsetBegin>3</CharacterOffsetBegin><CharacterOffsetEnd>9</CharacterOffsetEnd><POS>VVFIN</POS><NER>O</NER></token><token id=\"3\"><word>sich</word><lemma>sich</lemma><CharacterOffsetBegin>10</CharacterOffsetBegin><CharacterOffsetEnd>14</CharacterOffsetEnd><POS>PRF</POS><NER>O</NER></token><token id=\"4\"><word>als</word><lemma>als</lemma><CharacterOffsetBegin>15</CharacterOffsetBegin><CharacterOffsetEnd>18</CharacterOffsetEnd><POS>APPR</POS><NER>O</NER></token><token id=\"5\"><word>zweiter</word><lemma>zweiter</lemma><CharacterOffsetBegin>19</CharacterOffsetBegin><CharacterOffsetEnd>26</CharacterOffsetEnd><POS>ADJA</POS><NER>O</NER></token><token id=\"6\"><word>Paragraph</word><lemma>paragraph</lemma><CharacterOffsetBegin>27</CharacterOffsetBegin><CharacterOffsetEnd>36</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"7\"><word>.</word><lemma>.</lemma><CharacterOffsetBegin>36</CharacterOffsetBegin><CharacterOffsetEnd>37</CharacterOffsetEnd><POS>$.</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (S (PPER Er) (VVFIN fragte) (PRF sich)\n    (PP (APPR als) (ADJA zweiter) (NN Paragraph))\n    ($. .)))\n\n</parse></sentence></sentences></document></root>\r\n")]

result1D =

    [NLPtext{tz3loc = TextLoc{tlpage = "11", tlline = 6},
             tz3text = "(Krieg f\252r Welt)", tz3lang = German},
     NLPtext{tz3loc = TextLoc{tlpage = "12", tlline = 8},
             tz3text = "Unsere Br\228uche werden lebendig", tz3lang = German},
     NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 10},
             tz3text =
               "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
             tz3lang = German},
     NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 12},
             tz3text = "Er fragte sich als zweiter Paragraph.",
             tz3lang = German}]

