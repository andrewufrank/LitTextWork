-----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceText
-- Copyright   :  andrew u frank -
--
-- | not used when not producing text included
-- works with TZ1, language is marked
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Parser.ProduceLayout (module Parser.ProduceLayout
    ) where

import           Test.Framework
import           Data.Char               (toLower)
--import           Data.RDF  ()
import Data.RDF.Triple2text (triple2text)
--import           Data.RDF.Extension
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
--import           Parser.TextDescriptor  hiding ((</>))
import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB    -- result1A etc.
--import Lines2para.HandleLayout
    -- (RDFtypes (..), RDFproperties (..), TZ (..)
--      , TextDescriptor, PartURI, RDFsubj, Triple) -- TZ
--import Lines2para.Lines2ignore
--import Lines2para.Lines2para -- hiding ((</>))
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
import Uniform.TestHarness
import Producer.Servers (rdfBase)  -- from Foundation
import Parser.TextDescriptor hiding ((</>)) -- from Foundation
import Parser.NLPvocabulary

--gerastreeURI = "http://nlp.gerastree.at:9001/xtestx"
--gerastreeURI = "http://gerastree.at"
--layoutURItext =   gerastreeURI </> "layout_2017" :: PartURI
layoutURItext =   (showT rdfBase) </>  "layout_2017" :: PartURI

produceLayoutTriples ::  TextDescriptor -> [TZ1] -> [Triple]
-- test BAD -> J
-- put lines and pages into rdf
produceLayoutTriples textstate =
            concatMap (convOneTZ2triple textstate)


--buchURIx textstate = RDFsubj $ gerastreeURI
--            <#> authorDir textstate <-> buchName textstate
---- id of buch, part and sentence or page is attached

data LayoutType = Line | Page
    deriving (Show, Eq, Enum)

instance RDFtypes LayoutType where
    mkRDFtype p =  RDFtype $ layoutURItext <#> (toTitle . showT $ p)

data LayoutProperty = TomeNumber | LineNumber
    | PageNumber | LineText
            | InLineMarker | InLineMarkerPos
            -- could be label for text?
    deriving (Show, Eq, Enum)

instance RDFproperties LayoutProperty where
    mkRDFproperty p = RDFproperty
            $ layoutURItext <#> (toLowerStart . showT $ p)

--
newtype LineSigl = LineSigl RDFsubj
unLineSigl (LineSigl t) = t
--
formatLineID :: Int -> Text
formatLineID nr =   "L" <> (s2t .
        printf  ('%' : '0' : '5' : 'd' :[]) $  nr )
-- format to 5 digits
--
lineSigl :: TextDescriptor -> Int -> LineSigl
lineSigl textstate pn = LineSigl ( extendSlashRDFsubj
                (formatLineID  $ pn)
                      ( buchURIx $ textstate)
                      )

--newtype PageSigl = PageSigl RDFsubj
--unPageSigl (PageSigl t) = t
--
--formatPageID :: Text -> Text
--formatPageID nrtext =   "P" <> nrtext
---- format to 5 digits
----
--pageSigl :: TextState2 -> Text -> PageSigl
--pageSigl textstate pn = PageSigl ( extendSlashRDFsubj
--                (formatPageID  $ pn)
--                      ( buchURIx $ textstate)
--                      )

newtype InLineMarkerSigl = InLineMarkerSigl RDFsubj
unInLineMarkerSigl (InLineMarkerSigl t) = t

formatInLineMarker :: Int -> Text
formatInLineMarker nr  =   "ILM" <>
        (s2t . printf  ('%' : '0' : '2' : 'd' :[]) $  nr )
-- format to 5 digits
--
inLineMarkerSigl :: LineSigl -> Int -> InLineMarkerSigl
inLineMarkerSigl linesigl pn = InLineMarkerSigl ( extendSlashRDFsubj
                (formatInLineMarker  $ pn)
                      (unLineSigl linesigl)
                      )


debugTurtle = True

convOneTZ2triple :: TextDescriptor -> TZ1 -> [Triple]
-- produce all triples necessary for each line item
-- keeps only markup in v2
convOneTZ2triple textstate tz  = case tz of
--    TZzahl {}  -> errorT ["formParagraphs"
--                ,"should not have TZzahl left", showT tz]
    TZmarkup1{} -> lineTriple textstate tz
    TZleer1 {} -> [] -- where not elliminated?
------         errorT ["formParagraphs","should not leer", showT tz]
    TZtext1 {} -> lineTriple textstate tz
    TZignore1 {} -> []
    _  -> errorT ["convOneTZ2triple", "not expected type for T1", showT tz]


lineTriple :: TextDescriptor -> TZ1  -> [Triple]
-- ^ enter a line triple
-- processes markup as well - perhaps this is not necessary?
-- title and author in gutenberg does not have a language code
lineTriple textstate  tz =
    [ mkTripleInt (unLineSigl sigl) (mkRDFproperty LineNumber)
            (tlline . tzloc1 $ tz)
    , mkTripleType (unLineSigl sigl) (mkRDFtype Line)
    -- is not necessary, duck typing
    -- what has a lineNumber is a line
    , mkTriplePartOf (unLineSigl sigl)   (buchURIx textstate)
--    , mkTriplePartOf (unLineSigl sigl)   (unPageSigl pSigl)
    -- requires a page as an object
    -- gives the page number/text as it was parsed
    -- could be avoided if null
    , mkTripleLang3 (tzlang1 tz) (unLineSigl sigl)
            (mkRDFproperty LineText)
                    (twm . tztext1 $ tz)
    -- gives the text of a TZtext line
        ]
            ++ (concat . map (oneMarkerTriple sigl)
                $ (twmMarks . tztext1 $ tz))
            ++ pageNumberTriple
    where
        sigl = lineSigl textstate .  tlline . tzloc1$ tz
--        pSigl = pageSigl textstate . tlpage . tzloc $ tz
        pageNumberTriple = case tlpage . tzloc1 $ tz of
            Nothing -> []
            Just pgnr -> [  mkTripleText (unLineSigl sigl)
                    (mkRDFproperty PageNumber)
                    pgnr ]


--footnoteTriples :: TextState2 -> (Int,Text) -> [Triple]
---- make the extra triples for the marker
--footnoteTriples linesigl intText = if hasNoMarkers then []
--        else
--
--    where
--        hasNoMarkers =  null . twmMarks . tztext $ tz


oneMarkerTriple ::  LineSigl -> (Int,Text) -> [Triple]
oneMarkerTriple lineSigl intText =
    [ mkTripleText (unInLineMarkerSigl mSigl) (mkRDFproperty InLineMarker) (snd intText)
        , mkTripleInt (unInLineMarkerSigl mSigl) (mkRDFproperty InLineMarkerPos) (fst intText)
       , mkTriplePartOf (unInLineMarkerSigl mSigl) (unLineSigl lineSigl)
            ]
    where
        mSigl = inLineMarkerSigl lineSigl . fst $ intText


layoutTriples ::  TextDescriptor -> [TZ1] -> Text  -- test BAD -> J

--layoutTriples textstate =  unlines' .  map triple2text . produceLayoutTriples textstate
-- too expensive to map triple2text (at least on oporto)
layoutTriples textstate =  unlines' .  map showT . produceLayoutTriples textstate


test_1BAD_J = testVar3File result1A "resultBAD1" "resultJ1" layoutTriples
test_2BAD_J = testVar3File result2A "resultBAD2" "resultJ2" layoutTriples
test_3BAD_J = testVar3File result3A "resultBAD3" "resultJ3" layoutTriples
test_4BAD_J = testVar3File result4A "resultBAD4" "resultJ4" layoutTriples
test_5BAD_J = testVar3File result5A "resultBAD5" "resultJ5" layoutTriples
test_6BAD_J = testVar3File result6A "resultBAD6" "resultJ6" layoutTriples
----test_7BAD_J = testVar3File result7A "resultBAD7" "resultJ7" layoutTriples
test_8BAD_J = testVar3File result8A "resultBAD8" "resultJ8" layoutTriples
test_9BAD_J = testVar3File result9A "resultBAD9" "resultJ9" layoutTriples
test_10BAD_J = testVar3File result10A "resultBAD10" "resultJ10" layoutTriples




