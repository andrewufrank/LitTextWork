-----------------------------------------------------------------------------
--
-- Module      :  Parser . HandleLayout
-- Copyright   :  andrew u frank -
--
-- |  encode the layout on pages (lines, pages)
-- should pages become items (for rdf)?
-- line numbers are not including page number lines
-- should ignore lines not be put in rdf (yes - avoid pre and post stuff from gutenberg)
-- does not handle language yet
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--{-# OPTIONS_GHC -w #-}
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Lines2para.HandleLayout
    (module Lines2para.HandleLayout
--    , module Lines2para.MarkupText
--    , module LitTypes.TextDescriptor
--    , module Uniform.Error
    ) where

import Lines2para.MarkupText
import           Data.List.Split
--import           Uniform.Error
import LitTypes.TextDescriptor -- (TZ (..), tlline, tlpage , TextType (..))


paragraphs2TZlayout :: [TextZeile] -> [TZ]  -- test BA -> BB
-- ^ produce the paragraphs with the seitenzahlen in each line
paragraphs2TZlayout =
    removeSeitenZahlen . mergePara0  -- not yet done
    . distributePageNrs
    . etts2tzs  --  maps lines and numbers the  lines from start
    -- test BA -> BAA ... BAG -> C


ett2tz :: TextZeile -> TZ
-- convert the textzeilen to tz without filling location
-- some markup is converted to lit text
ett2tz (TextZeile ty t) = TZtext {tzt=ty, tztext = t, tzloc = zero  }
--ett2tz (ZahlZeile t) = TZzahl {tztext = t, tzloc = zero, tzlang=zero}
ett2tz (MarkupZeile BuchIgnoreLine t) = TZignore {tztext = t,  tzloc = zero}
ett2tz (MarkupZeile BuchGedicht t) =
    TZtext {tzt=Kurz0, tztext = t,  tzloc = zero }
ett2tz (MarkupZeile BuchEnde _) = TZleer {tzloc = zero}
-- will be filtered out
ett2tz (MarkupZeile tok t) = TZmarkup {tztext = t, tztok = tok, tzloc = zero
--        , tzlang=zero
--        , tlpara = zero , tzInPart = zero
             }
ett2tz LeerZeile = TZleer {tzloc = zero}
ett2tz (NeueSeite) = TZneueSeite {tzloc = zero}
ett2tz x = errorT ["ett2tz not prepared for pattern", showT x]

etts2tzs :: [TextZeile] -> [TZ]
-- maps the text zeilen and numbers the lines from 1 to end
etts2tzs = zipWith line2tz [1..] . map ett2tz

line2tz :: Int -> TZ -> TZ
-- ^ fill a TZ with the linenumber
line2tz i tz = tz {tzloc = (tzloc tz) {tlline =  i} }


unparseTZs :: [TZ] -> Text
-- produce a text which can be written to a file and compared with the original
unparseTZs = concat' . map renderZeile

mergePara0 :: [TZ] -> [TZ]
-- ^ merge two Para0 broken by page break
mergePara0 = id
--mergePara0 (t1:t2:t3:ts) =
--                if (tzt t1 ==Para0)
----                 (TZtext {tzt=Zahl0}) : ((TZtext {tzt=Para0})
--                    then merge2para0 t1 t3 : mergePara0 (t3:ts)
--                    else t1 : mergePara0 (t2:t3:ts)
--
--merge2Para0 t1 t3 =

removeSeitenZahlen :: [TZ] -> [TZ]
removeSeitenZahlen = filter (not . isSeitenzahl) . filter (not . isNeueSeite)

-------------------------PAGES

distributePageNrs :: [TZ] -> [TZ]
-- mark the zeilen with the page number
-- leave pagenumber to deal with when merging para0 text
-- page numbers are asumed at he bottom of the page!
    -- if no pagenumbers found then all content is in the first sublist
distributePageNrs  =   concat .   markSublist . pages
    where
        pages :: [TZ] -> [[TZ]]
        pages tz = (split .  keepDelimsR . whenElt) isSeitenzahl tz
            -- (TZzahl zero zero : tz)
        -- to start with a page 0  -- break after page number
        -- appends the page to the sublist
        pageNrOfSublist :: [TZ] -> Maybe Text
        pageNrOfSublist tzs =
            if isSeitenzahl lasttz
                    then Just . zeilenText  $ lasttz -- isSeitenzahl
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
        markSublist2 sl = markTZsWithPage
--                    (fromJustNote "distributePageNrs" $
                        ( pageNrOfSublist sl)
                                    (init sl)

markTZsWithPage :: Maybe Text -> [TZ] -> [TZ]
-- put the page number into the list
markTZsWithPage i  = map  (\tz -> tz {tzloc = (tzloc tz) {tlpage = i} } )

checkSeitenzahl [] = []
checkSeitenzahl (t:ts) =
    if isSeitenzahl t
            then errorT ["checkSeitenzahl found one ", showT t]
            else t : checkSeitenzahl ts

instance Zeilen TZ where
    isLeerzeile TZleer  {} = True
    isLeerzeile _          = False

    isSeitenzahl (TZtext {tzt = Zahl0}) = True
    isSeitenzahl _          = False

    isTextZeile (TZtext {tzt = tyt}) = tyt `elem` [Text0, Para0, Kurz0]
    isTextZeile _         = False

    isTextZeileIncludeInPara  (TZtext {tzt=Text0}) = True   -- can include footnote text
    isTextZeileIncludeInPara  (TZtext {tzt=Kurz0}) = True
    isTextZeileIncludeInPara _             = False

    isMarkupZeile TZmarkup {} = True
    isMarkupZeile _           = False

    isKurzeZeile (TZtext {tzt=Kurz0}) = True
    isKurzeZeile _         = False

    isNeueSeite TZneueSeite {} = True
    isNeueSeite _ = False

    isMarkupX code TZmarkup{tztok=c} =  code == c
    isMarkupX _ _                 = False

    zeilenText TZleer {} = ""
    zeilenText (TZtext {tztext=tx}) =  twm tx
    zeilenText _ = ""

    renderZeile  _ = error "render zeile in HandleLayout not implemented"
--    renderZeile  tz =  case tz of
--        TZleer {} -> ""
--        TZzahl {} -> errorT ["renderZeile seitenzahl", "should not occur", showT tz]
--        TZmarkup {tztok=tok} -> unwords' ["." <> markerPure (tztok tz), tztext tz] <>
----                        if tok == BuchHL1 then
--                                " - para " <> (unparaID . tlpara . tzloc $ tz) <>
--                                " in " <> (unparaID . tzInPart $ tz) <>
----                                " tok "
----                                (markerPure . tztok $ tz) <>  "  text"
----                                (tztext $ tz) <>
--                                "\n"
----                                            else "\n"
--        -- leerzeile nach markup
--        -- TZkurz {} ->  tztext tz <> "\n"
--        TZtext {} ->  tztext tz <> "\n"
--        TZpara {} -> "\n" <> (unparaID . tlpara . tzloc $ tz)
--                <> " in " <> (unparaID . tzInPart $ tz)
--                <> " on page " <> (tlpage . tzloc $ tz)
--                <> "\n" <> (concat' . map renderZeile  . tztzs $ tz) <> "\n"

instance Zeilen TZ1 where
    zeilenText TZleer1 {} = ""
    zeilenText TZtext1 {tztext1=tx} =  getText . twm1 $ tx
    zeilenText _ = ""

    isTextZeileIncludeInPara  TZtext1 {tzt1=Text0} = True   -- can include footnote text
    isTextZeileIncludeInPara  TZtext1 {tzt1=Kurz0} = True
    isTextZeileIncludeInPara _             = False

    isKurzeZeile TZtext1 {tzt1=Kurz0} = True
    isKurzeZeile _         = False
    -- what is needed?



