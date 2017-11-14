 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FormNLPsnips  D -> DA
-- Copyright   :  andrew u frank -
--
-- | form pieces of literal text which are reasonably sized

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.FormNLPsnips
--    (module Parser.ProduceDocCallNLP
--    , module CoreNLP.Defs0
--    , module Lines2para.Lines2para
--    , module Producer.Servers
--    , module Parser.FilterTextForNLP
--    )
    where

import           Test.Framework
import Uniform.TestHarness
import Data.Maybe -- todo
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo  -- for test
import Producer.Servers
import           CoreNLP.Defs0
import CoreNLP.CoreNLPxml (readDocString)
import Data.List.Split
import Uniform.HttpCallWithConduit (makeHttpPost7, addPort2URI)
import Text.Regex (mkRegex, subRegex)
import Parser.FilterTextForNLP

formSnips :: [NLPtext] -> [NLPtext]
-- collect paragraphis in reasonalbe snips for NLP processing
formSnips [] = []
formSnips [n] = [n]
formSnips (n1:n2:ns) =  case mergeNLPtext n1 n2 of
        Nothing -> n1 : formSnips (n2:ns)
        Just ab ->  formSnips (ab:ns)


minSnipSize = 5000 -- char
maxSnipSize = 10000

test_1_D_DA = testFile2File "resultD1" "resultDA1" formSnips
test_2_D_DA = testFile2File "resultD2" "resultDA2" formSnips
test_3_D_DA = testFile2File "resultD3" "resultDA3" formSnips
test_4_D_DA = testFile2File "resultD4" "resultDA4" formSnips
test_5_D_DA = testFile2File "resultD5" "resultDA5" formSnips
test_6_D_DA = testFile2File "resultD6" "resultDA6" formSnips
test_8_D_DA = testFile2File "resultD8" "resultDA8" formSnips
test_9_D_DA = testFile2File "resultD9" "resultDA9" formSnips
test_10_D_DA = testFile2File "resultD10" "resultDA10" formSnips

mergeNLPtext :: NLPtext -> NLPtext -> Maybe NLPtext
-- merge two text if same language and size less than maxSnipSize
mergeNLPtext a b = if (sameLang a b && alength + blength < maxSnipSize)
                        then Just (a{tz3text = tz3text a <> " " <> tz3text b})
                        else Nothing

    where
        alength = lengthChar . tz3text $ a
        blength = lengthChar . tz3text $ b
        sameLang a b = tz3lang a == tz3lang b

-- test mergeNLP
text1 = NLPtext {tz3loc = TextLoc {tlpage = Just "7", tlline = 59}, tz3para = ParaNum 11,
        tz3text = "Neben dem Spiegel hing in einem Rahmen eine Portraitaufnahme .", tz3lang = German}
text2 = NLPtext {tz3loc = TextLoc {tlpage = Just "7", tlline = 59}, tz3para = ParaNum 11,
        tz3text = "die ich dann mit Schminke korrigierte.", tz3lang = German}
t12 =  Just
  (NLPtext{tz3loc = TextLoc{tlpage = Just "7", tlline = 59},
           tz3para = ParaNum 11,
           tz3text =
             "Neben dem Spiegel hing in einem Rahmen eine Portraitaufnahme . die ich dann mit Schminke korrigierte.",
           tz3lang = German})

test_mergeNLP = assertEqual    t12  (mergeNLPtext text1 text2)

-- old for splitting

--nlpDocSizeLimit = 5000  -- 18,000 gives timeout for brest
---- there seems to be an issue with texts which are exactly 5000 and the next piece is
---- then empty, which then loops infinitely calling nlp with input ""
--
--
----splitAndTryAgain :: Bool -> Bool -> URI -> [(Text,Text)] -> Text -> ErrIO [(Doc0)]
------ split the text in two and try each
----splitAndTryAgain debugNLP showXML nlpServer vars text = do
----    when debugNLP $ putIOwords ["splitAndTryAgain start"]
----    -- this will not be used
----    return []
--
--textid :: Text -> Text
--textid = id
--
--
--textSplit :: Text -> [Text]
----textSplit =  fromJustNote "textSplit" . splitOn' ". "
---- append . to end last piece
--textSplit = fmap s2t . split (keepDelimsR $ onSublist ". "  ) . t2s
---- statt onSublis elt verwenden, so das "?." oder "!." auch brechen
---- asked on stack overflow how to combine... (coreNLP breaks on ? as well)
---- issue: splits for "costs 1s. per year" and similar
---- could be controled by merging pieces when number "s." is at end of piece
---- regular expression used in  geany could be used? is this save?
--
--textSplit2 :: Text -> [Text]
---- split on "I" - in joyce is sort of start of a new idea..
--    -- prepend I to start next piece
--textSplit2 = fmap s2t . split (keepDelimsL $ onSublist " I "  ) . t2s
--
--
--getPiece :: Int -> [Text] -> [Text]
---- get a piece of length < 2000
--getPiece limit  = chop (takePiece limit "")
--
----chop :: ([a] -> (b, [a])) -> [a] -> [b]
----A useful recursion pattern for processing a list to produce a new list,
----often used for "chopping" up the input list.
----Typically chop is called with some function that will
----consume an initial prefix of the list and produce a value and the rest of the list.
--
--takePiece :: Int -> Text  -> [Text] ->   (Text, [Text])
--takePiece _ b [] = (b,[])
--takePiece limit b (a:as)
--    | null' b && lengthChar a > limit = (a, as)
--    | lengthChar ab > limit = (b, a:as)
--    | otherwise = takePiece limit ab as  -- accumulate a larger piece
--                    where
--                        ab = concat' [b, " ", a]
----                        limit = 12000
--
----test_longText1 ::  IO ()
----test_longText1 = testFile2File "longtext.txt" "lt1" textid
----test_split = testFile2File "lt1" "lt2" textSplit
----test_chop = testFile2File "lt2" "lt3" getPiece
--
---- test with limit 5
----test_getPiece1 = assertEqual  ["abcdefg", " hik"] (getPiece ["abcdefg", "hik"])
----test_getPiece2 = assertEqual [" abc", " defg", " hik"] (getPiece ["abc","defg", "hik"])
----test_getPiece3 = assertEqual  [" AB", "abcdefg", " hik"] (getPiece ["AB","abcdefg", "hik"])
----test_getPiece4 = assertEqual  [" AB", " abc", " d ef", " g hi", " k"]
----            (getPiece ["AB","abc", "d", "ef","g", "hi","k"])

