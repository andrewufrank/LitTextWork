-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines
-- Copyright   :  andrew u frank -
--
-- |  grouping the lines to paragraphs  - completes the parsing
-- TextZeilen is reading in , TZ is a conversion of TextZeilen (no IO)
-- works only on text lines
-- unparse the internal TZ representation and produce a tile to compare with the
--original txt file
-- does not show the page numbers ???
-- seitenzahlen must be numbers (not alpha) - is used to parse!
-- .ende is necessary to distribute page numbers!
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Lines2para.Lines2para
    (module Lines2para.Lines2para
--    , module Lines2para.Lines2ignore
--    , module Lines2para.HandleLayout
        ) where

--import Lines2para.Lines2ignore
import  Lines2para.HandleLayout -- TZ

import           Data.List.Split
import           Uniform.Error
import           Uniform.Strings     hiding ((<|>), (</>))
import Uniform.FileIO
-- TODO string s
import Data.List (nub)
import           Test.Framework
import Uniform.TestHarness

newtype ParaNum = ParaNum Int deriving (Read, Show, Eq)
-- just to avoid confusions
unparaNum (ParaNum t) = t
instance Zeros ParaNum where zero =  ParaNum zero

-- the format accumulation all detail info to build the triples.
-- only tzpara and tzmarkup in final result
data TZ2 =
     TZ2para  {tz2loc :: TextLoc, tz2tzs :: [TZ], tz2lang :: LanguageCode
            , tz2para :: ParaNum
            , tz2inPart :: ParaNum}
    | TZ2markup  {tz2loc :: TextLoc, tz2text:: TextWithMarks
                    , tz2tok :: BuchToken, tz2lang :: LanguageCode
                    , tz2para :: ParaNum
                    , tz2inPart :: ParaNum
                    }
            deriving (Read, Show, Eq )

--paragraphs2TZ :: [TextZeilen] -> [TZ2]  -- test BA -> C
---- ^ produce the text files (ignores removed, language marked)
---- paragraphs formed etc.  (all together in LinesToParagraph)
---- page number and line numbers are in layout
--paragraphs2TZ =
--    paragraphs2TZpara . paragraphs2TZsimple . paragraphs2TZlayout

paragraphsTZ2TZ2 :: [TZ] -> [TZ2]  -- test BA -> C
-- ^ produce the text files (ignores removed, language marked)
-- paragraphs formed etc.  (all together in LinesToParagraph)
-- page number and line numbers are in layout
paragraphsTZ2TZ2 =
    paragraphs2TZpara
--            . paragraphs2TZsimple -- now in layout

paragraphs2TZpara :: [TZ] -> [TZ2]  -- test BA -> C
-- ^ produce the text files (ignores removed, language marked)
-- but not paragraphs
-- page number and line numbers are in layout
paragraphs2TZpara =
--- ^ produce the paragraphs with the seitenzahlen in each line
--- and the header linked
    distributeHeader . markParaNr .
--    filterAlleLeer .  -- these are not used after forming paras
    formParagraphs
        -- test BAD -> BAE ...   -> C



--test_0BA_BAC = testFile2File "resultBA0" "resultBAC0" paragraphs2TZpara
test_1BAD_BAE = testFile2File "resultBAD1" "resultBAE1" paragraphsTZ2TZ2
test_2BAD_BAE = testFile2File "resultBAD2" "resultBAE2" paragraphsTZ2TZ2
test_3BAD_BAE = testFile2File "resultBAD3" "resultBAE3" paragraphsTZ2TZ2
test_4BAD_BAE = testFile2File "resultBAD4" "resultBAE4" paragraphsTZ2TZ2
test_5BAD_BAE = testFile2File "resultBAD5" "resultBAE5" paragraphsTZ2TZ2
test_6BAD_BAE = testFile2File "resultBAD6" "resultBAE6" paragraphsTZ2TZ2
test_8BAD_BAE = testFile2File "resultBAD8" "resultBAE8" paragraphsTZ2TZ2
test_9BAD_BAE = testFile2File "resultBAD9" "resultBAE9" paragraphsTZ2TZ2

--test_8B_BAE = testFile2File "resultBA8" "resultBAE8" paragraphs2TZ


----------- PARA

formParagraphs :: [TZ] -> [TZ2]
-- grouplines to meaningful paragraphs (for nlp)
formParagraphs [] = []
--formParagraphs [t] = [t]
formParagraphs (t:ts) = case t of
    TZleer {} -> formParagraphs ts  -- removes empty lines
    TZneueSeite {}  -> errorT ["formParagraphs","should not have TZneueSeite left", showT t]
    TZignore {} -> formParagraphs ts  -- removes ignore lines

    TZmarkup {..} -> TZ2markup {tz2loc=tzloc, tz2text=tztext
                    , tz2tok=tztok, tz2lang=tzlang
                    , tz2para = zero, tz2inPart=zero} : formParagraphs ts

    TZtext {tzt=Zahl0}  -> errorT ["formParagraphs","should not have TZzahl left", showT t]

    TZtext {tzt=Text0} -> p : formParagraphs rest
                        where (p,rest) = collectPara (t:ts)
    TZtext {tzt=Para0} -> p : formParagraphs ts  -- do not collect, is one line per paragraph
            where p = collectInParagrah [t]
    TZtext {tzt=Kurz0} -> p : formParagraphs rest
                        where (p,rest) = collectKurz (t:ts)
    TZtext {tzt=Fussnote0} ->  formParagraphs ts   -- is this ok? dropping t
            -- form a paragraph with the footnote text

    otherwise -> errorT ["formParagraph - other ", showT t]
    -- allCaps ?  fussnote

--formParagraphs x = errorT ["formParagraph - outer  ", showT x]


collectPara :: [TZ] -> (TZ2, [TZ])
-- group longest poossible chain
collectPara  tzs
        | null rest = (collectInParagrah ts, [])
        | isKurzeZeile h  =  (collectInParagrah (ts ++ [h]), tail rest)
    --    | isParaZeile h (collectInParagraph ts,
        | otherwise = (collectInParagrah ts, rest)  -- how to deal with texts of one line per paragraph

    where
        (ts, rest) = span isTextZeileIncludeInPara tzs  -- isTextZeileNotPara does not includes ParaO
        h = headNote "headCollectPara" $ rest
-- TODO string
lastChar :: Text -> Maybe Char
lastChar t = if null' t then Nothing else Just . headNote "lastChar" . t2s$ t

collectInParagrah :: [TZ] -> TZ2
-- collect the text lines in a paragraph
collectInParagrah [] = errorT ["collectInParagrah ", "should not occur with empty list"]
collectInParagrah tzs =
    TZ2para {tz2tzs  = tzs
           , tz2loc = TextLoc
                {tlpage = tlpage . tzloc . headNote "collectInParagrah" $ tzs

                , tlline = tlline . tzloc . headNote "collectInParagrah 2" $ tzs
                }
           , tz2para = zero
           , tz2lang = tzlang . headNote "collectInParagrah3" $ tzs
           -- could check that all have the same langauges
           , tz2inPart = zero  -- this is the id of the title? check that the titel has this
        }

collectKurz :: [TZ] -> (TZ2, [TZ])
-- group longest poossible chain, including merging paragraph
-- paragraphs broken by seitenzahl is not merged - should go here?
--   issue: long paragraph lines must not be merged. this is property of the text
-- possible approach - mark lines as paralines in such texts
collectKurz  tzs
    | null rest = (collectInParagrah ts, [])
    | isKurzeZeile h  = (collectInParagrah (ts ++ [h]), tail rest)
    | otherwise = (collectInParagrah ts, rest)

    where
        (ts, rest) = span isKurzeZeile tzs
        h = headNote "headCollectPara kurz" $ rest

filterAlleLeer :: [TZ] -> [TZ]
filterAlleLeer = filter notLeer
    where
            notLeer (TZleer {}) = False
            notLeer (TZneueSeite {}) = False
            notLeer (TZmarkup {tztext=t}) = not . null' . twm $ t
            notLeer (TZtext {tztext=t}) = not . null' . twm $ t
            notLeer _ = True
-----------------------------------

markParaNr :: [TZ2] -> [TZ2]
---- put paragrah numbers in (all TZ items are paragraphs, unless collected)
markParaNr = zipWith markOnePara  [1..]

markOnePara :: Int -> TZ2 -> TZ2
markOnePara nr tz@TZ2para {} = tz { tz2para = ParaNum nr}
markOnePara nr tz@TZ2markup {} = tz { tz2para = ParaNum nr}
--markOnePara _ tz = error (show tz)


--------------- HEADERS
instance Zeilen TZ2 where
    isMarkupX code TZ2markup{tz2tok=c} =  code == c
    isMarkupX code _                 = False

    -- combinet the zeilen to a single paragraph
    -- adds a " " at end of line
    -- adds blanks before punctation marks and at end of the paragraph

    zeilenText TZ2markup {tz2text=tx} = twm tx
    zeilenText (TZ2para {tz2tzs=ts}) =  concat'
        . map (\s -> append s " ") .  map zeilenText
                 $ ts
--    zeilenText _ = ""

distributeHeader = distributeHeader2 BuchTitel

distributeHeader2 :: BuchToken -> [TZ2] -> [TZ2]
-- mark the TZ with the immediately preceding header
distributeHeader2  tok [] = []
distributeHeader2  tok tzs = concat  .  markSublistHeader . chapters $ tzs
    where
        chapters :: [TZ2] -> [[TZ2]]
        chapters = (split .  keepDelimsL . whenElt) (isMarkupX tok)

        markSublistHeader :: [[TZ2]] -> [[TZ2]]
        -- the first must not be marked, the rest
        markSublistHeader [] = errorT ["markSublistHeader2", "empty list of sublist should not occur"]

        markSublistHeader (s1: sl0)  = s1 : map markSublistHeaderLower sl0

        markSublistHeaderLower [] = []
        markSublistHeaderLower sl1 = getHeader sl1 : (
                 (\sl3 -> maybe sl3 (\tok2 -> distributeHeader2 tok2 sl3)
                                                (lowerHeader tok))
                .   markTZsWithHeader (tz2para .  getHeader $ sl1 )
                 )
                (tail sl1)

        getHeader = headNote "distributeHeaders2"

markTZsWithHeader :: ParaNum -> [TZ2] -> [TZ2]
markTZsWithHeader p []           = [] -- errorT ["markTZsWithHeader", "empty list should not occur", showT p]
markTZsWithHeader headerPara tzs = map  (markoneheader headerPara) tzs
--markTZsWithHeader p t = errorT ["markTZsWithHeader", "should not occur2", showT p]

markoneheader headerPara tz@TZ2para{} =   tz {tz2inPart = headerPara}
markoneheader headerPara tz@TZ2markup{} = tz {tz2inPart = headerPara}

markoneheader a b = errorT ["markoneheader", showT a, showT b,
        "at this stage in the transformation, only para, markup and leer should occur"]

lowerHeader BuchTitel = Just BuchHL1
lowerHeader BuchHL1   = Just BuchHL2
lowerHeader BuchHL2   = Just BuchHL3
lowerHeader BuchHL3   = Nothing
lowerHeader l         = errorT ["lowerHeader", "for ", showT l]


-- test text combinatioin zeilenText

--test_zeilenText = do
--    let res = map zeilenText t11
--    assertEqual t1_res res

--t1_res =
--    ["'Fury said to a mouse, That he met in the house. ",
--     "CHAPTER IV. The Rabbit Sends in a Little Bill",
--     "It was the White Rabbit, trotting slowly back again, and looking anxiously about as it went, as if it had lost something . "]
--
--
--t11 :: [TZ2]
--t11 =
--[TZ2para{tz2loc = TextLoc{tlpage = "", tlline = 49},
--             tz2tzs =
--               [TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 50},
--                       tztext = TextWithMarks{twm = "'Fury said to a", twmMarks = []},
--                       tzlang = English},
--                TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 51},
--                       tztext = TextWithMarks{twm = "mouse, That he", twmMarks = []},
--                       tzlang = English},
--                TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 52},
--                       tztext = TextWithMarks{twm = "met in the", twmMarks = []},
--                       tzlang = English},
--                TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 53},
--                       tztext = TextWithMarks{twm = "house.", twmMarks = []},
--                       tzlang = English}],
--             tz2lang = English, tz2para = ParaNum 9, tz2inPart = ParaNum 4},
--    TZ2markup{tz2loc = TextLoc{tlpage = "", tlline = 55},
--               tz2text =
--                 TextWithMarks{twm =
--                                 "CHAPTER IV. The Rabbit Sends in a Little Bill",
--                               twmMarks = []},
--               tz2tok = BuchHL1, tz2lang = English, tz2para = ParaNum 10,
--               tz2inPart = ParaNum 1},
--     TZ2para{tz2loc = TextLoc{tlpage = "", tlline = 57},
--             tz2tzs =
--               [TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 57},
--                       tztext =
--                         TextWithMarks{twm =
--                                         "It was the White Rabbit, trotting slowly back again, and looking",
--                                       twmMarks = []},
--                       tzlang = English},
--                TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 58},
--                       tztext =
--                         TextWithMarks{twm =
--                                         "anxiously about as it went, as if it had lost something .",
--                                       twmMarks = []},
--                       tzlang = English}],
--             tz2lang = English, tz2para = ParaNum 11, tz2inPart = ParaNum 10}]


