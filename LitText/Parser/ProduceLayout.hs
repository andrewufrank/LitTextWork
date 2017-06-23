-----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceText
-- Copyright   :  andrew u frank -
--
-- |
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
import           Data.RDF
import Data.RDF.Triple2text (triple2text)
import           Data.RDF.Extension
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import           Parser.Foundation  hiding ((</>))
import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB
import Lines2para.HandleLayout -- TZ
--import Lines2para.Lines2ignore
--import Lines2para.Lines2para -- hiding ((</>))
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
import Uniform.TestHarness


layoutURItext =   gerastreeURI </> "layout_2017" :: PartURI

produceLayoutTriples ::  TextState2 -> [TZ] -> [Triple]  -- test BAD -> J
-- put lines and pages into rdf
produceLayoutTriples textstate = concatMap (convOneTZ2triple textstate)


buchURIx textstate = RDFsubj $ gerastreeURI
            <#> authorText textstate <-> buchnameText textstate
-- id of buch, part and sentence or page is attached

data LayoutType = Line | Page
    deriving (Show, Eq, Enum)

instance RDFtypes LayoutType where
    mkRDFtype p =  RDFtype $ layoutURItext <#> (toTitle . showT $ p)

data LayoutProperty = TomeNumber | LineNumber | PageNumber | LineText  -- could be label?
    deriving (Show, Eq, Enum)

instance RDFproperties LayoutProperty where
    mkRDFproperty p = RDFproperty $ layoutURItext <#> (toLowerStart . showT $ p)

--
newtype LineSigl = LineSigl RDFsubj
unLineSigl (LineSigl t) = t
--
formatLineID :: Int -> Text
formatLineID nr =   "L" <> (s2t . printf  ('%' : '0' : '5' : 'd' :[]) $  nr )
-- format to 5 digits
--
lineSigl :: TextState2 -> Int -> LineSigl
lineSigl textstate pn = LineSigl ( extendSlashRDFsubj
                (formatLineID  $ pn)
                      ( buchURIx $ textstate)
                      )
newtype PageSigl = PageSigl RDFsubj
unPageSigl (PageSigl t) = t

formatPageID :: Text -> Text
formatPageID nrtext =   "P" <> nrtext
-- format to 5 digits
--
pageSigl :: TextState2 -> Text -> PageSigl
pageSigl textstate pn = PageSigl ( extendSlashRDFsubj
                (formatPageID  $ pn)
                      ( buchURIx $ textstate)
                      )


debugTurtle = True

convOneTZ2triple :: TextState2 -> TZ -> [Triple]
-- produce all triples necessary for each line item
convOneTZ2triple textstate tz  = case tz of
--    TZzahl {}  -> errorT ["formParagraphs","should not have TZzahl left", showT tz]
    TZmarkup {} -> lineTriple textstate tz
    TZleer {} -> [] -- where not elliminated?
------         errorT ["formParagraphs","should not leer", showT tz]
    TZtext {} -> lineTriple textstate tz
    TZignore {} -> []
    _  -> errorT [showT tz]


lineTriple :: TextState2 -> TZ  -> [Triple]
-- ^ enter a line triple
lineTriple textstate  tz =
    [ mkTripleInt (unLineSigl sigl) (mkRDFproperty LineNumber)  (tlline . tzloc $ tz)
    , mkTripleType (unLineSigl sigl) (mkRDFtype Line)
    -- is not necessary, duck typing - what has a lineNumber is a line
    , mkTriplePartOf (unLineSigl sigl)   (buchURIx textstate)
--    , mkTriplePartOf (unLineSigl sigl)   (unPageSigl pSigl)
    -- requires a page as an object
    , mkTripleText (unLineSigl sigl) (mkRDFproperty PageNumber)  (tlpage . tzloc $ tz)
    -- gives the page number/text as it was parsed
    , mkTripleLang (tzlang tz) (unLineSigl sigl) (mkRDFproperty LineText)  (twm . tztext $ tz)
    -- gives the text of a TZtext line
        ]
    where
        sigl = lineSigl textstate .  tlline . tzloc $ tz
        pSigl = pageSigl textstate . tlpage . tzloc $ tz

layoutTriples ::  TextState2 -> [TZ] -> Text  -- test BAD -> J

layoutTriples textstate =  unlines' .  map triple2text . produceLayoutTriples textstate


test_1BAD_J = testVar3File result1A "resultBAD1" "resultJ1" layoutTriples
test_2BAD_J = testVar3File result2A "resultBAD2" "resultJ2" layoutTriples
test_3BAD_J = testVar3File result3A "resultBAD3" "resultJ3" layoutTriples
test_4BAD_J = testVar3File result4A "resultBAD4" "resultJ4" layoutTriples
test_5BAD_J = testVar3File result5A "resultBAD5" "resultJ5" layoutTriples
test_6BAD_J = testVar3File result6A "resultBAD6" "resultJ6" layoutTriples
--test_7BAD_J = testVar3File result7A "resultBAD7" "resultJ7" layoutTriples
--test_8BAD_J = testVar3File result8A "resultBAD8" "resultJ8" layoutTriples




