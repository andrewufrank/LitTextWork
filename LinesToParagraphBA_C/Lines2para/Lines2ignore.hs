-----------------------------------------------------------------------------
--
-- Module      :  Parser . deal with ignored lines
-- Copyright   :  andrew u frank -
--
-- |  grouping the lines to paragraphs  - completes the parsing
-- TextZeilen is reading in , TZ is a conversion of TextZeilen (no IO)
-- works only on text lines
-- unpare the internal TZ representation and produce a tile to compare with the
--original txt file
-- does not show the page numbers
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
{-# OPTIONS_GHC -w #-}

module Lines2para.Lines2ignore
    (module Lines2para.Lines2ignore
    , module Lines2para.HandleLayout

    ) where
--    (htf_thisModulesTests   -- for tests
--
--    ,  paragraphs2TZlit
---- other exports are for Lines2paraTests:
----    , formParagraphs
----    , distributeIgnore
--    , distributeLanguage
----    , distributePageNrs
----    , etts2tzs
----    , distributeHeader
----    , markParaNr
----    , filterZeilen
----    , TZ (..), TextLoc (..), ParaID (..), unparaID
--        )  where


--import BuchCode.MarkupText
--import BuchCode.BuchToken
import Lines2para.HandleLayout

import           Data.List.Split
--import           Parser.Foundation   hiding ((</>)) -- gives TZ
--import           Uniform.Error
--import           Uniform.Strings  --   hiding ((<|>), (</>))
--import Uniform.FileIO
-- TODO string s
--import Data.List (nub)
--import           Text.Printf         (printf)
import           Test.Framework



paragraphs2TZsimple :: [TZ] -> [TZ]  -- test BA -> C
-- ^ produce the text files (ignores removed, language marked)
-- but not paragraphs
-- page number and line numbers are in layout
paragraphs2TZsimple =
    map fixTextAllCaps .
    distributeLanguage .
    distributeIgnore
    -- test BA -> BAD

fixTextAllCaps ::  TZ  ->  TZ
fixTextAllCaps TZtext {tzt=AllCaps0, ..} = TZmarkup{tzloc = tzloc
            , tztext=tztext, tztok= BuchHL2, tzlang=tzlang}
            -- can later be dealt with similar to language
            -- with a switch in the markup file
fixTextAllCaps tz = tz


-- test the first (expected ok) part of the chain
test_1_BA_BAD =do
        putIOwords ["test_1_BA_BAD", "from result1BA_tz_markupResult1 to result1BAD"]
        assertEqual result1BAD
            (paragraphs2TZsimple  result1BAC)
test_2_BA_BAD =do
        putIOwords ["test_2_BA_BAD", "from result2BA_tz_markupResult1 to result2BAD"]
        assertEqual result2BAD
                (paragraphs2TZsimple result2BAC)
test_6_BA_BAD =do
        putIOwords ["test_6_BA_BAD", "from result6BA_tz_markupResult1 to result6BAD"]
        assertEqual result6BAD
                (paragraphs2TZsimple result6BAC)

------------LANGUAGE

distributeLanguage :: [TZ] -> [TZ]
-- mark the zeilen with the language
-- removes the language markup lines
distributeLanguage tz0 = concat . markSublistLanguage . pages $ tz0
    where
        pages :: [TZ] -> [[TZ]]
        pages  = (split .  keepDelimsL . whenElt) isLanguageCode
--      prepends
        isLanguageCode TZmarkup{tztok=BuchSprache} = True
        isLanguageCode _                 = False

        getLangCode _ tz3@TZmarkup{tztext=tx}  =
                    readLanguageCode "distributeLanguage" .twm $ tx
        getLangCode l tz3 = errorT ["distributeLanguage - languageCode 2"
                , showT tz3, "\n"
                , unlines' . map showT $ l
                , "fulllist is \n", unlines' . map showT $ tz0]

        markSublistLanguage :: [[TZ]] -> [[TZ]]
        markSublistLanguage []       = []
        markSublistLanguage (s1:sl1) = s1 : map markSublistLanguage2 sl1
        -- s1 is the partial start sublist with no language code

        markSublistLanguage2 :: [TZ] -> [TZ]
        markSublistLanguage2 [] = []
        markSublistLanguage2 sl2 = markTZsWithLanguage
                (getLangCode sl2 . headNote "distributeLanguage" $ sl2) (tail sl2)

readLanguageCode :: Text -> Text -> LanguageCode
-- ^ read the code for the language German, Deutsch, Englisch
--readLanguageCode  = readNoteT
-- todo move to parser
readLanguageCode _ "Deutsch" = German
readLanguageCode msg l  = readNoteT msg l

markTZsWithLanguage :: LanguageCode -> [TZ] -> [TZ]
-- put the page number into the list
markTZsWithLanguage lg = map  (markoneLanguage lg)
    where
        markoneLanguage lg tz@TZtext {} = tz {tzlang = lg }
        markoneLanguage lg tz@TZmarkup {} = tz {tzlang = lg }
        markoneLanguage lg tz = tz

------- distribute ignore to  ignore end TODO
distributeIgnore :: [TZ] -> [TZ]
-- mark the zeilen with the Ignore
distributeIgnore  = concat . markSublistIgnore . pages
    where
        pages :: [TZ] -> [[TZ]]
        pages = (split .  keepDelimsL . whenElt) isIgnoreCode
--      prepends
        isIgnoreCode TZmarkup{tztok=BuchIgnoreTo} = True
        isIgnoreCode TZmarkup{tztok=BuchIgnoreEnd} = True
        isIgnoreCode _            = False

        -- getLangCode _ tz3@TZmarkup{}  = readIgnoreCode "distributeIgnore" . tztext $ tz3
        -- getLangCode l tz3 = errorT ["distributeIgnore - IgnoreCode 2", showT tz3, "\n"
        --         , unlines' . map showT $ l
        --         , "fulllist is \n", unlines' . map showT $ tz0]

        markSublistIgnore :: [[TZ]] -> [[TZ]]
        markSublistIgnore []       = []
        markSublistIgnore (s1:sl1) = s1 : map markSublistIgnore2 sl1
        -- s1 is the partial start sublist with no Ignore code

        markSublistIgnore2 :: [TZ] -> [TZ]
        markSublistIgnore2 [] = []
        markSublistIgnore2 (s2: sl2) = case tztok s2 of
        -- a list of sublist with a ignore at start
                    BuchIgnoreTo  -> markTZsWithIgnore sl2  -- this is a sublist to ignore
                    BuchIgnoreEnd -> sl2  -- this is to keep
                    _             -> errorT ["markSublistIgnore2", showT s2]

-- readIgnoreCode :: Text -> Text -> IgnoreCode
-- readIgnoreCode msg t = readNoteT msg t

markTZsWithIgnore ::  [TZ] -> [TZ]
-- this should convert all lines, independent to ignorezeile
--markTZsWithIgnore  = map  (\tz1 -> TZignore {tzloc = tzloc tz1, tztext = tztext tz1, tzlang = tzlang tz1 } )
markTZsWithIgnore  = map  markoneWithIgnore
    where
        markoneWithIgnore  tz1@TZtext {} =
                TZignore {tzloc = tzloc tz1, tztext = tztext tz1 }
--        markoneWithIgnore  tz1@TZmarkup {} =
--                TZignore {tzloc = tzloc tz1, tztext = tztext tz1 }
-- keep the markup codes in the ignore blocks, simplifies markup
-- possibly automatic from guttenberg texts
        markoneWithIgnore  tz1 = tz1

#include "Lines2ignoreTestResults.res"

{-
----------- PARA

formParagraphs :: [TZ] -> [TZ]
-- grouplines to meaningful paragraphs (for nlp)
formParagraphs [] = []
formParagraphs [t] = [t]
formParagraphs (t:ts) = case t of
    TZzahl {}  -> errorT ["formParagraphs","should not have TZzahl left", showT t]
    TZneueSeite {}  -> errorT ["formParagraphs","should not have TZneueSeite left", showT t]
    TZmarkup {} -> t : formParagraphs ts
    TZleer {} -> formParagraphs ts  -- removes empty lines
    TZignore {} -> formParagraphs ts  -- removes ignore lines
    TZtext {} -> p : formParagraphs rest
                        where (p,rest) = collectPara (t:ts)
    TZparaZeile {} -> p : formParagraphs ts
            where p = collectInParagrah [t]
    TZkurz {} -> p : formParagraphs rest
                        where (p,rest) = collectKurz (t:ts)
    otherwise -> errorT ["formParagraph - other ", showT t]

--formParagraphs x = errorT ["formParagraph - outer  ", showT x]


collectPara :: [TZ] -> (TZ, [TZ])
-- group longest poossible chain
collectPara  tzs
    | null rest = (collectInParagrah ts, [])
    | isKurzeZeile h  =  (collectInParagrah (ts ++ [h]), tail rest)
    | otherwise = (collectInParagrah ts, rest)  -- here the issue

    where
        (ts, rest) = span isTextZeile tzs
        h = headNote "headCollectPara" $ rest
-- TODO string
lastChar :: Text -> Maybe Char
lastChar t = if null' t then Nothing else Just . headNote "lastChar" . t2s$ t

collectInParagrah :: [TZ] -> TZ
-- collect the text lines in a paragraph
collectInParagrah [] = errorT ["collectInParagrah ", "should not occur with empty list"]
collectInParagrah tzs =
    TZpara {tztzs  = tzs
           , tzloc = TextLoc
                {tlpage = tlpage . tzloc . headNote "collectInParagrah" $ tzs
--                , tlpara = formatParaID 0
                , tlline = tlline . tzloc . headNote "collectInParagrah 2" $ tzs
                }
           , tzlang = tzlang . headNote "collectInParagrah3" $ tzs
           -- could check that all have the same langauges
           , tzInPart = zero  -- this is the id of the title? check that the titel has this
        }

collectKurz :: [TZ] -> (TZ, [TZ])
-- group longest poossible chain, including merging paragraph
-- paragraphs broken by seitenzahl is not merged - should go here?
collectKurz  tzs
    | null rest = (collectInParagrah ts, [])
    | isKurzeZeile h  =  (collectInParagrah (ts ++ [h]), tail rest)
    | otherwise = (collectInParagrah ts, rest)

    where
        (ts, rest) = span isKurzeZeile tzs
        h = headNote "headCollectPara kurz" $ rest

markParaNr :: [TZ] -> [TZ]
---- put paragrah numbers in (all TZ items are paragraphs, unless collected)
markParaNr = zipWith markOnePara  [1..]

markOnePara :: Int -> TZ -> TZ
markOnePara nr tz = tz {tzloc = (tzloc tz) {tlpara = formatParaID nr} }


--------------- HEADERS
distributeHeader = distributeHeader2 BuchTitel

distributeHeader2 :: BuchToken -> [TZ] -> [TZ]
-- mark the TZ with the immediately preceding header
distributeHeader2  tok [] = []
distributeHeader2  tok tzs = concat  .  markSublistHeader . chapters $ tzs
    where
        chapters :: [TZ] -> [[TZ]]
        chapters = (split .  keepDelimsL . whenElt) (isMarkupX tok)

        markSublistHeader :: [[TZ]] -> [[TZ]]
        -- the first must not be marked, the rest
        markSublistHeader [] = errorT ["markSublistHeader2", "empty list of sublist should not occur"]

        markSublistHeader (s1: sl0)  = s1 : map markSublistHeaderLower sl0

        markSublistHeaderLower [] = []
        markSublistHeaderLower sl1 = getHeader sl1 : (
                         (\sl3 -> maybe sl3 (\tok2 -> distributeHeader2 tok2 sl3) (lowerHeader tok)) .
                         markTZsWithHeader (tlpara . tzloc . getHeader $ sl1 )
                         )
                        (tail sl1)

        getHeader = headNote "distributeHeaders2"

--isHeader tz = isMarkupX BuchTitel tz || isMarkupX BuchHL1 tz
--            || isMarkupX BuchHL2 tz || isMarkupX BuchHL3 tz

markTZsWithHeader :: ParaID -> [TZ] -> [TZ]
markTZsWithHeader p []           = [] -- errorT ["markTZsWithHeader", "empty list should not occur", showT p]
markTZsWithHeader headerPara tzs = map  (markoneheader headerPara) tzs
--markTZsWithHeader p t = errorT ["markTZsWithHeader", "should not occur2", showT p]

markoneheader headerPara tz@TZpara{} = tz {tzInPart = headerPara}
markoneheader headerPara tz@TZmarkup{} = tz {tzInPart = headerPara}
markoneheader headerPara tz@TZleer{} = tz
markoneheader headerPara tz = errorT ["markoneheader", showT headerPara, showT tz,
        "at this stage in the transformation, only para, markup and leer should occur"]

lowerHeader BuchTitel = Just BuchHL1
lowerHeader BuchHL1   = Just BuchHL2
lowerHeader BuchHL2   = Just BuchHL3
lowerHeader BuchHL3   = Nothing
lowerHeader l         = errorT ["lowerHeader", "for ", showT l]




--filterZeilen :: [TZ] -> [TZ]
---- ^ remove some lines - here the neueSeite, where i have no idea what to do with
--filterZeilen = filter (not.isNeueSeite)
-}

