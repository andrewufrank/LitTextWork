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
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--{-# OPTIONS_GHC -w #-}

module LitText.Lines2para.Lines2para
    (module LitText.Lines2para.Lines2para
    , TZ2
        ) where


import           Data.List.Split
import Data.List (nub)
import LitText.Lines2para.HandleLayout    -- instance for zeilen TZ1

paragraphsTZ2TZ2 :: [TZ1] -> [TZ2]  -- test C -> CA
-- ^ produce the text files (ignores removed, language marked)
-- paragraphs formed etc.  (all together in LinesToParagraph)
-- page number and line numbers are in layout
paragraphsTZ2TZ2 = paragraphs2TZpara
--            . paragraphs2TZsimple -- now in layout

paragraphs2TZpara :: [TZ1] -> [TZ2]  -- test BA -> C
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



----------- PARA

formParagraphs :: [TZ1] -> [TZ2]
-- grouplines to meaningful paragraphs (for nlp)
formParagraphs [] = []
--formParagraphs [t] = [t]
formParagraphs (t:ts) = case t of
    TZleer1 {} -> formParagraphs ts  -- removes empty lines
    TZneueSeite1 {}  -> errorT ["formParagraphs","should not have TZneueSeite left", showT t]
    TZignore1 {} -> formParagraphs ts  -- removes ignore lines

    TZmarkup1 {..} -> TZ2markup {tz2loc=tzloc1, tz2text=tztext1
                    , tz2tok=tztok1 -- , tz2lang=tzlang1
                    , tz2para = zero, tz2inPart=zero} : formParagraphs ts

    TZtext1 {tzt1=Zahl0}  -> errorT ["formParagraphs","should not have TZzahl left", showT t]

    TZtext1 {tzt1=Text0} -> p : formParagraphs rest
                        where (p,rest) = collectPara (t:ts)
    TZtext1 {tzt1=Para0} -> p : formParagraphs ts  -- do not collect, is one line per paragraph
            where p = collectInParagrah [t]
    TZtext1 {tzt1=Kurz0} -> p : formParagraphs rest
                        where (p,rest) = collectKurz (t:ts)
    TZtext1 {tzt1=Fussnote0} ->  formParagraphs ts   -- is this ok? dropping t
            -- form a paragraph with the footnote text

    otherwise -> errorT ["formParagraph - other ", showT t]
    -- allCaps ?  fussnote

--formParagraphs x = errorT ["formParagraph - outer  ", showT x]


collectPara :: [TZ1] -> (TZ2, [TZ1])
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

collectInParagrah :: [TZ1] -> TZ2
-- collect the text lines in a paragraph
collectInParagrah [] = errorT ["collectInParagrah ", "should not occur with empty list"]
collectInParagrah tzs =
    TZ2para {tz2tzs  = tzs
           , tz2loc = TextLoc
                {tlpage = tlpage . tzloc1 . headNote "collectInParagrah" $ tzs

                , tlline = tlline . tzloc1 . headNote "collectInParagrah 2" $ tzs
                }
           , tz2para = zero
--           , tz2lang = tzlang1 . headNote "collectInParagrah3" $ tzs
           -- could check that all have the same langauges
           , tz2inPart = zero  -- this is the id of the title? check that the titel has this
        }

collectKurz :: [TZ1] -> (TZ2, [TZ1])
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

filterAlleLeer :: [TZ1] -> [TZ1]
filterAlleLeer = filter notLeer
    where
            notLeer (TZleer1 {}) = False
            notLeer (TZneueSeite1 {}) = False
            notLeer (TZmarkup1 {tztext1=t}) = notNullLC . twm1 $ t
            notLeer (TZtext1 {tztext1=t}) = notNullLC . twm1 $ t
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

    zeilenText TZ2markup {tz2text=tx} = getText $ twm1 tx
    zeilenText (TZ2para {tz2tzs=ts}) =  concat'
        . map (\s -> append s " ") .  map zeilenText
                 $ ts
--    zeilenText _ = ""


distributeHeader  ::   [TZ2] -> [TZ2]
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



