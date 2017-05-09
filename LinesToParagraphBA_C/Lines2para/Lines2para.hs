-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines
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
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Lines2para.Lines2para (paragraphs2TZ
-- other exports are for Lines2paraTests:
    , formParagraphs
    , distributeIgnore , distributeLanguage , distributePageNrs , etts2tzs
    , distributeHeader, markParaNr
    , filterZeilen
    , TZ (..), TextLoc (..), ParaID (..), unparaID
        )  where


import BuchCode.MarkupText (Zeilen (..), TextZeilen (..))
import BuchCode.BuchToken (LanguageCode (..), BuchToken (..), markerPure)
import           Data.List.Split
--import           Parser.Foundation   hiding ((</>)) -- gives TZ
import           Uniform.Error
import           Uniform.Strings     hiding ((<|>), (</>))
import Uniform.FileIO
-- TODO string s
import Data.List (nub)
import           Text.Printf         (printf)

newtype ParaID = ParaID Text deriving (Show, Eq)
-- just to avoid confusions
unparaID (ParaID t) = t
instance Zeros ParaID where zero = formatParaID zero

formatParaID :: Int -> ParaID
formatParaID nr = ParaID $ "P" <> (s2t . printf  ('%' : '0' : '5' : 'd' :[]) $  nr )
-- format to 5 digits

formatLineID :: Int -> Text
formatLineID nr = "L" <> (s2t . printf  ('%' : '0' : '3' : 'd' :[]) $  nr )
-- format to 3 digits

data TextLoc = TextLoc {tlpage :: Text, tlpara :: ParaID, tlline :: Text} deriving (Show, Eq)
-- ^ the place of a line in the full text
-- for simplification, all counts are from the start of the text
-- not relative to the page or paragraph (can be computed, if desired)
-- page number (tlline) is text, to deal with III etc.


instance Zeros TextLoc where zero = TextLoc zero zero zero

data TZ =  TZtext {tzloc :: TextLoc, tztext:: Text, tzlang :: LanguageCode}
        | TZzahl  {tzloc :: TextLoc, tztext:: Text, tzlang :: LanguageCode}
        | TZmarkup  {tzloc :: TextLoc, tztext:: Text
                        , tztok :: BuchToken, tzlang :: LanguageCode, tzInPart :: ParaID}
        | TZkurz  {tzloc :: TextLoc, tztext:: Text, tzlang :: LanguageCode}
        -- a short line, can be merged at the end of a paragraph (but only one)
        | TZparaZeile  {tzloc :: TextLoc, tztext:: Text, tzlang :: LanguageCode}
--        -- ^ a line which is a paragraph, not mergable with other
--        -- could be used for poems as well
--        -- but usually poems are automatically recognized by short lines
        | TZleer  {tzloc :: TextLoc, tzlang :: LanguageCode}
        | TZneueSeite  {tzloc :: TextLoc, tzlang :: LanguageCode}
        | TZpara  {tzloc :: TextLoc, tztzs :: [TZ], tzlang :: LanguageCode, tzInPart :: ParaID}
        | TZignore {tzloc :: TextLoc, tztext:: Text, tzlang :: LanguageCode}
              -- para can be short (gedicht) or long (text) lines
                -- in tzloc only tlpara is valid
           deriving (Show, Eq )

instance Zeros TZ where zero = TZleer zero zero


unparseTZs :: [TZ] -> Text
-- produce a text which can be written to a file and compared with the original
unparseTZs = concat' . map renderZeile

paragraphs2TZ :: [TextZeilen] -> [TZ]  -- test BA -> C
-- ^ produce the paragraphs with the seitenzahlen in each line
-- and the header linked
paragraphs2TZ = distributeHeader . markParaNr . formParagraphs
    . distributeIgnore . distributeLanguage . distributePageNrs
    . filterZeilen . etts2tzs
    -- test BA -> BAA ... BAG -> C


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
                , tlpara = formatParaID 0
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

-------------------------PAGES

distributePageNrs :: [TZ] -> [TZ]
-- mark the zeilen with the page number
-- no pagenumber left
distributePageNrs  =  checkSeitenzahl . concat .   markSublist . pages
    where
        pages :: [TZ] -> [[TZ]]
        pages tz = (split .  keepDelimsR . whenElt) isSeitenzahl tz
            -- (TZzahl zero zero : tz)
        -- to start with a page 0  -- break after page number
        -- appends the page to the sublist
        pageNrOfSublist :: [TZ] -> Maybe Text
        pageNrOfSublist tzs = if isSeitenzahl lasttz then Just . zeilenText  $ lasttz -- isSeitenzahl
                                            else Nothing
            where
                lasttz = last tzs

        markSublist :: [[TZ]] -> [[TZ]]
        markSublist []  = []
        markSublist sls =  map markSublist2 (init sls) ++ [last sls]
--            [markSublist2 $ head sls ] ++ (map markSublist2 . tail $ sls)
--the last sublist contains just the end mark
        markSublist2 :: [TZ] -> [TZ]
        markSublist2 [] = []
        markSublist2 sl = markTZsWithPage (fromJustNote "distributePageNrs" $ pageNrOfSublist sl)
                                    (init sl)

markTZsWithPage :: Text -> [TZ] -> [TZ]
-- put the page number into the list
markTZsWithPage i  = map  (\tz -> tz {tzloc = (tzloc tz) {tlpage = i} } )

checkSeitenzahl [] = []
checkSeitenzahl (t:ts) = if isSeitenzahl t then errorT ["checkSeitenzahl found one ", showT t]
                                else t : checkSeitenzahl ts
------- distribute ignore to  ignore end TODO
distributeIgnore :: [TZ] -> [TZ]
-- mark the zeilen with the Ignore
distributeIgnore  = concat . markSublistIgnore . pages
    where
        pages :: [TZ] -> [[TZ]]
        pages = (split .  keepDelimsL . whenElt) isIgnoreCode
--      prepends
        isIgnoreCode tz2@TZmarkup{} = BuchIgnoreTo == tztok tz2 || BuchIgnoreEnd  == tztok tz2
        isIgnoreCode tz2            = False

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
markTZsWithIgnore  = map  (\tz1 -> TZignore {tzloc = tzloc tz1, tztext = tztext tz1, tzlang = tzlang tz1 } )

------------LANGUAGE

distributeLanguage :: [TZ] -> [TZ]
-- mark the zeilen with the language
distributeLanguage tz0 = concat . markSublistLanguage . pages $ tz0
    where
        pages :: [TZ] -> [[TZ]]
        pages  = (split .  keepDelimsL . whenElt) isLanguageCode
--      prepends
        isLanguageCode tz2@TZmarkup{} = BuchSprache == tztok tz2
        isLanguageCode tz2            = False

        getLangCode _ tz3@TZmarkup{}  = readLanguageCode "distributeLanguage" . tztext $ tz3
        getLangCode l tz3 = errorT ["distributeLanguage - languageCode 2", showT tz3, "\n"
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
readLanguageCode _ "Deutsch" = German
readLanguageCode msg l  = readNoteT msg l

markTZsWithLanguage :: LanguageCode -> [TZ] -> [TZ]
-- put the page number into the list
markTZsWithLanguage lg = map  (\tz -> tz {tzlang = lg } )

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

instance Zeilen TZ where
    isLeerzeile TZleer  {} = True
    isLeerzeile _          = False

    isSeitenzahl  TZzahl {} = True
    isSeitenzahl _          = False

    isTextZeile TZtext {} = True
    isTextZeile _         = False

    isMarkupZeile TZmarkup {} = True
    isMarkupZeile _           = False

    isKurzeZeile TZkurz {} = True
    isKurzeZeile _         = False

    isNeueSeite TZneueSeite {} = True
    isNeueSeite _ = False

    isMarkupX code TZmarkup{tztok=c} =  code == c
    isMarkupX code _                 = False

    zeilenText TZleer {} = ""
    zeilenText TZpara {tztzs=tok} = fromJustNote "zeilenText " . intercalate' "\n" . map zeilenText $ tok
    zeilenText t = tztext  t

    renderZeile  tz =  case tz of
        TZleer {} -> ""
        TZzahl {} -> errorT ["renderZeile seitenzahl", "should not occur", showT tz]
        TZmarkup {tztok=tok} -> unwords' ["." <> markerPure (tztok tz), tztext tz] <>
--                        if tok == BuchHL1 then
                                " - para " <> (unparaID . tlpara . tzloc $ tz) <>
                                " in " <> (unparaID . tzInPart $ tz) <>
--                                " tok "
--                                (markerPure . tztok $ tz) <>  "  text"
--                                (tztext $ tz) <>
                                "\n"
--                                            else "\n"
        -- leerzeile nach markup
        -- TZkurz {} ->  tztext tz <> "\n"
        TZtext {} ->  tztext tz <> "\n"
        TZpara {} -> "\n" <> (unparaID . tlpara . tzloc $ tz)
                <> " in " <> (unparaID . tzInPart $ tz)
                <> " on page " <> (tlpage . tzloc $ tz)
                <> "\n" <> (concat' . map renderZeile  . tztzs $ tz) <> "\n"


etts2tzs :: [TextZeilen] -> [TZ]
-- gives line numbers
etts2tzs = zipWith line2tz [1..] . map ett2tz

ett2tz :: TextZeilen -> TZ
-- convert the et to tz without filling location
ett2tz (TextZeile t) = TZtext {tztext = t, tzloc = zero, tzlang=zero}
ett2tz (ZahlZeile t) = TZzahl {tztext = t, tzloc = zero, tzlang=zero}
ett2tz (MarkupZeile BuchIgnore t) = TZignore {tztext = t,  tzloc = zero, tzlang=zero}
ett2tz (MarkupZeile BuchGedicht t) = TZkurz {tztext = t,  tzloc = zero, tzlang=zero}
ett2tz (MarkupZeile BuchEnde t) = TZleer {tzloc = zero, tzlang=zero}
-- will be filtered out
-- keep the gedicht mark content as kuzre zeile, TODO could be processed as marker
-- for parts where the layout should be kept
ett2tz (MarkupZeile tok t) = TZmarkup {tztext = t, tztok = tok, tzloc = zero, tzlang=zero, tzInPart=zero}
-- ett2tz (KurzeZeile t) = TZkurz {tztext = t, tzloc = zero, tzlang=zero}
ett2tz LeerZeile = TZleer {tzloc = zero, tzlang=zero}
ett2tz (KurzeZeile t) = TZkurz {tztext = t,  tzloc = zero, tzlang=zero}
ett2tz (ParaZeile t) = TZparaZeile {tztext = t,  tzloc = zero, tzlang=zero}
ett2tz (NeueSeite) = TZneueSeite {tzloc = zero,  tzlang=zero}
ett2tz x = errorT ["ett2tz not prepared for pattern", showT x]

filterZeilen :: [TZ] -> [TZ]
-- ^ remove some lines - here the neueSeite, where i have no idea what to do with
filterZeilen = filter (not.isNeueSeite)

line2tz :: Int -> TZ -> TZ
-- ^ fill a TZ with the linenumber
line2tz i tz = tz {tzloc = (tzloc tz) {tlline = formatLineID i} }
