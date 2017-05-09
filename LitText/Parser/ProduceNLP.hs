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
    (convertTZ2nlp
    , prepareTZ4nlp
    , ErrIO (..)
    , htf_thisModulesTests
    , result1E_nlpResult
    ) where

import           Test.Framework

--import           CoreNLP.Snippets2nt          (makeNLPrequest5) -- , readDocString)
import           Data.RDF
-- import           Data.RDF.Extension
--import           Data.Text.Encoding           (decodeLatin1, encodeUtf8)
import           Parser.Foundation
import Lines2para.Lines2para
import           Lines2para.Lines2paraTests
--    (result1C_tzResult1, result1A_textstate)  -- for testing
import Parser.ReadMarkupAB

--import           Parser.ProduceLit            (buchURIx, paraSigl)
--import           Uniform.Convenience.StartApp
--import           Uniform.FileIO
import           Uniform.Strings              hiding ((<|>))
import Uniform.Error
import Uniform.HttpGet
--import Parser.ProduceNLPtriples

debugNLP = False


prepareTZ4nlp :: [TZ] -> [TZ]  -- test C  -> D
-- selecte the text from TZ and convert to text
prepareTZ4nlp = map formatParaText . filter condNLPtext
        ---------------------------------preparing for analysis

condNLPtext :: TZ -> Bool
-- select the paragraphs and the titles to TZtext
condNLPtext tz  = case tz of
    TZzahl {}  -> errorT ["condNLPtext","should not have TZzahl left", showT tz]
    TZmarkup {} ->
            case tztok tz of
                BuchTitel ->  True
                BuchHL1   ->  True
                BuchHL2   ->  True
                BuchHL3   ->  True
                _         ->   False
--                    buchEnde ... errorT ["ProduceLit.hs conv2", "missing Markup case", showT . tztok $ tz]
    TZleer {} -> False  -- errorT ["condNLPtext","should not haveleer", showT tz]
    TZtext {} -> errorT ["condNLPtext","should not have single text", showT tz]
    TZkurz {} -> errorT ["condNLPtext","should not have single kurz", showT tz]
    TZpara {} -> True

--    TZkurz {} -> p : condNLPtext rest
--                        where (p,rest) = collectKurz (t:ts)
--    _ -> errorT ["ProduceLit.hs conv2", "missing TZ case", showT tz]

formatParaText :: TZ -> TZ
-- convert the headers to a tztext
formatParaText tz@TZpara{} = TZtext {tzloc = tzloc tz, tzlang = tzlang tz
        , tztext = foldl1 combine2linesWithHyphenation . map tztext $ (tztzs tz)
        }

formatParaText tz  = tz

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
    let t1 = prepareTZ4nlp result1C
--    putIOwords ["prepareTZ4nlp: result (for next) ", s2t $ show t1]
--    putIOwords ["prepareTZ4nlp:  result ", show' t1]
    assertEqual result1D_tznlpResult t1


-------------------------------------------------D -> E

convertTZ2nlp :: PartURI -> TZ -> ErrIO (TZ,Text)   -- the xml to analyzse  D -> E
-- send a tz text to coreNLP server
-- works on individual paragraphs
convertTZ2nlp sloc tz = do
    when debugNLP $ putIOwords ["convertTZ2nlp"]

    let language = tzlang tz
    let text = tztext tz

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



test_1_D_E_convertTZ2nlp ::   IO ()  -- D -> D
test_1_D_E_convertTZ2nlp =  do
    putIOwords ["convertTZ2nlp:   "] -- tzResult]
    let sloc = serverLoc result1A
    res <- mapM (runErr . convertTZ2nlp sloc) result1D_tznlpResult
--    putIOwords ["prepareTZ4nlp: result (for next) ", s2t $ show t1]
--    putIOwords ["prepareTZ4nlp:  result ", show' t1]
    assertEqual result1E_nlpResult res

result1D_tznlpResult =
    [TZmarkup{tzloc =
                TextLoc{tlpage = "11", tlpara = ParaID "P00002", tlline = "L006"},
              tztext = "(Krieg f\252r Welt)", tztok = BuchTitel, tzlang = German,
              tzInPart = ParaID "P00000"},
     TZmarkup{tzloc =
                TextLoc{tlpage = "12", tlpara = ParaID "P00003", tlline = "L008"},
              tztext = "Unsere Br\228uche werden lebendig", tztok = BuchHL1,
              tzlang = German, tzInPart = ParaID "P00002"},
     TZtext{tzloc =
              TextLoc{tlpage = "13", tlpara = ParaID "P00004", tlline = "L010"},
            tztext =
              "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
            tzlang = German},
     TZtext{tzloc =
              TextLoc{tlpage = "13", tlpara = ParaID "P00005", tlline = "L012"},
           tztext = "Er fragte sich als zweiter Paragraph.", tzlang = German}]

result1E_nlpResult =
    [Right
       (TZmarkup{tzloc =
                   TextLoc{tlpage = "11", tlpara = ParaID "P00002", tlline = "L006"},
                 tztext = "(Krieg f\252r Welt)", tztok = BuchTitel, tzlang = German,
                 tzInPart = ParaID "P00000"},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>-LRB-</word><lemma>-lrb-</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>1</CharacterOffsetEnd><POS>TRUNC</POS><NER>O</NER></token><token id=\"2\"><word>Krieg</word><lemma>krieg</lemma><CharacterOffsetBegin>1</CharacterOffsetBegin><CharacterOffsetEnd>6</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"3\"><word>f\252r</word><lemma>f\252r</lemma><CharacterOffsetBegin>7</CharacterOffsetBegin><CharacterOffsetEnd>10</CharacterOffsetEnd><POS>APPR</POS><NER>O</NER></token><token id=\"4\"><word>Welt</word><lemma>welt</lemma><CharacterOffsetBegin>11</CharacterOffsetBegin><CharacterOffsetEnd>15</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"5\"><word>-RRB-</word><lemma>-rrb-</lemma><CharacterOffsetBegin>15</CharacterOffsetBegin><CharacterOffsetEnd>16</CharacterOffsetEnd><POS>TRUNC</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (NUR\n    (S\n      (NP\n        (CNP (TRUNC -LRB-) (NN Krieg))\n        (PP (APPR f\252r) (NN Welt)))\n      (VP\n        (CVP\n          (VP (TRUNC -RRB-)))))))\n\n</parse></sentence></sentences></document></root>\r\n"),
     Right
       (TZmarkup{tzloc =
                   TextLoc{tlpage = "12", tlpara = ParaID "P00003", tlline = "L008"},
                 tztext = "Unsere Br\228uche werden lebendig", tztok = BuchHL1,
                 tzlang = German, tzInPart = ParaID "P00002"},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>Unsere</word><lemma>unsere</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>6</CharacterOffsetEnd><POS>PPOSAT</POS><NER>O</NER></token><token id=\"2\"><word>Br\228uche</word><lemma>br\228uche</lemma><CharacterOffsetBegin>7</CharacterOffsetBegin><CharacterOffsetEnd>14</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"3\"><word>werden</word><lemma>werden</lemma><CharacterOffsetBegin>15</CharacterOffsetBegin><CharacterOffsetEnd>21</CharacterOffsetEnd><POS>VAFIN</POS><NER>O</NER></token><token id=\"4\"><word>lebendig</word><lemma>lebendig</lemma><CharacterOffsetBegin>22</CharacterOffsetBegin><CharacterOffsetEnd>30</CharacterOffsetEnd><POS>ADJD</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (NUR\n    (S\n      (NP (PPOSAT Unsere) (NN Br\228uche))\n      (VAFIN werden) (ADJD lebendig))))\n\n</parse></sentence></sentences></document></root>\r\n"),
     Right
       (TZtext{tzloc =
                 TextLoc{tlpage = "13", tlpara = ParaID "P00004", tlline = "L010"},
               tztext =
                 "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
               tzlang = German},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>Was</word><lemma>was</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>3</CharacterOffsetEnd><POS>PWS</POS><NER>O</NER></token><token id=\"2\"><word>w\252rde</word><lemma>w\252rde</lemma><CharacterOffsetBegin>4</CharacterOffsetBegin><CharacterOffsetEnd>9</CharacterOffsetEnd><POS>VAFIN</POS><NER>O</NER></token><token id=\"3\"><word>ihm</word><lemma>ihm</lemma><CharacterOffsetBegin>10</CharacterOffsetBegin><CharacterOffsetEnd>13</CharacterOffsetEnd><POS>PPER</POS><NER>O</NER></token><token id=\"4\"><word>fremd</word><lemma>fremd</lemma><CharacterOffsetBegin>14</CharacterOffsetBegin><CharacterOffsetEnd>19</CharacterOffsetEnd><POS>ADJD</POS><NER>O</NER></token><token id=\"5\"><word>und</word><lemma>und</lemma><CharacterOffsetBegin>20</CharacterOffsetBegin><CharacterOffsetEnd>23</CharacterOffsetEnd><POS>KON</POS><NER>O</NER></token><token id=\"6\"><word>was</word><lemma>was</lemma><CharacterOffsetBegin>24</CharacterOffsetBegin><CharacterOffsetEnd>27</CharacterOffsetEnd><POS>PWS</POS><NER>O</NER></token><token id=\"7\"><word>m\246chte</word><lemma>m\246chte</lemma><CharacterOffsetBegin>28</CharacterOffsetBegin><CharacterOffsetEnd>34</CharacterOffsetEnd><POS>VMFIN</POS><NER>O</NER></token><token id=\"8\"><word>sein</word><lemma>sein</lemma><CharacterOffsetBegin>35</CharacterOffsetBegin><CharacterOffsetEnd>39</CharacterOffsetEnd><POS>PPOSAT</POS><NER>O</NER></token><token id=\"9\"><word>eigen</word><lemma>eigen</lemma><CharacterOffsetBegin>40</CharacterOffsetBegin><CharacterOffsetEnd>45</CharacterOffsetEnd><POS>ADJD</POS><NER>O</NER></token><token id=\"10\"><word>sein</word><lemma>sein</lemma><CharacterOffsetBegin>46</CharacterOffsetBegin><CharacterOffsetEnd>50</CharacterOffsetEnd><POS>VAINF</POS><NER>O</NER></token><token id=\"11\"><word>in</word><lemma>in</lemma><CharacterOffsetBegin>51</CharacterOffsetBegin><CharacterOffsetEnd>53</CharacterOffsetEnd><POS>APPR</POS><NER>O</NER></token><token id=\"12\"><word>C\233rb\232re</word><lemma>c\233rb\232re</lemma><CharacterOffsetBegin>54</CharacterOffsetBegin><CharacterOffsetEnd>61</CharacterOffsetEnd><POS>NN</POS><NER>I-LOC</NER></token><token id=\"13\"><word>?</word><lemma>?</lemma><CharacterOffsetBegin>61</CharacterOffsetBegin><CharacterOffsetEnd>62</CharacterOffsetEnd><POS>$.</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (CS\n    (S\n      (NP (PWS Was))\n      (VAFIN w\252rde) (PPER ihm) (ADJD fremd))\n    (KON und)\n    (S (PWS was) (VMFIN m\246chte)\n      (VP\n        (NP\n          (CNP\n            (NP (PPOSAT sein)\n              (CNP\n                (NP\n                  (AP (ADJD eigen)))))))\n        (VAINF sein)\n        (PP (APPR in) (NN C\233rb\232re))))\n    ($. ?)))\n\n</parse></sentence></sentences></document></root>\r\n"),
     Right
       (TZtext{tzloc =
                 TextLoc{tlpage = "13", tlpara = ParaID "P00005", tlline = "L012"},
               tztext = "Er fragte sich als zweiter Paragraph.", tzlang = German},
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n<?xml-stylesheet href=\"CoreNLP-to-HTML.xsl\" type=\"text/xsl\"?>\r\n<root><document><sentences><sentence id=\"1\"><tokens><token id=\"1\"><word>Er</word><lemma>er</lemma><CharacterOffsetBegin>0</CharacterOffsetBegin><CharacterOffsetEnd>2</CharacterOffsetEnd><POS>PPER</POS><NER>O</NER></token><token id=\"2\"><word>fragte</word><lemma>fragte</lemma><CharacterOffsetBegin>3</CharacterOffsetBegin><CharacterOffsetEnd>9</CharacterOffsetEnd><POS>VVFIN</POS><NER>O</NER></token><token id=\"3\"><word>sich</word><lemma>sich</lemma><CharacterOffsetBegin>10</CharacterOffsetBegin><CharacterOffsetEnd>14</CharacterOffsetEnd><POS>PRF</POS><NER>O</NER></token><token id=\"4\"><word>als</word><lemma>als</lemma><CharacterOffsetBegin>15</CharacterOffsetBegin><CharacterOffsetEnd>18</CharacterOffsetEnd><POS>APPR</POS><NER>O</NER></token><token id=\"5\"><word>zweiter</word><lemma>zweiter</lemma><CharacterOffsetBegin>19</CharacterOffsetBegin><CharacterOffsetEnd>26</CharacterOffsetEnd><POS>ADJA</POS><NER>O</NER></token><token id=\"6\"><word>Paragraph</word><lemma>paragraph</lemma><CharacterOffsetBegin>27</CharacterOffsetBegin><CharacterOffsetEnd>36</CharacterOffsetEnd><POS>NN</POS><NER>O</NER></token><token id=\"7\"><word>.</word><lemma>.</lemma><CharacterOffsetBegin>36</CharacterOffsetBegin><CharacterOffsetEnd>37</CharacterOffsetEnd><POS>$.</POS><NER>O</NER></token></tokens><parse>(ROOT\n  (S (PPER Er) (VVFIN fragte) (PRF sich)\n    (PP (APPR als) (ADJA zweiter) (NN Paragraph))\n    ($. .)))\n\n</parse></sentence></sentences></document></root>\r\n")]


