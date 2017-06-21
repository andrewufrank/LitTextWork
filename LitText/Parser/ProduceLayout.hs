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
--import           Data.RDF.Extension
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import           Parser.Foundation  hiding ((</>))
import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB
import Lines2para.Lines2para -- hiding ((</>))
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
import Uniform.TestHarness


layoutURItext =   gerastreeURI </> "layout_2017" :: PartURI

produceLayoutTriples ::  TextState2 -> [TZ] -> [Triple]  -- test C=BAE -> H
-- put lines and pages into rdf
produceLayoutTriples textstate = concatMap (convOneTZ2triple textstate)


buchURIx textstate = RDFsubj $ gerastreeURI
            <#> authorText textstate <-> buchnameText textstate
-- id of buch, part and sentence or page is attached

--data LayoutProperty = IsTome | IsPage | IsLine
--        deriving (Show, Eq, Enum)
--
--instance RDFproperties LayoutProperty where
--    mkRDFproperty p = RDFproperty $  layoutURItext <#> (toLowerStart . showT $ p)
--instance RDFproperties BuchToken where
--    mkRDFproperty t = RDFproperty $  layoutURItext <#> (toLowerStart . markerPure $ t)

-- toLowerStart :: Text -> Text
-- -- convert the first character to lowercase
-- toLowerStart t = (toLower . T.head $ t ) `cons` (T.tail t)
data LayoutType = Line | Page
    deriving (Show, Eq, Enum)

instance RDFtypes LayoutType where
    mkRDFtype p =  RDFtype $ layoutURItext <#> (toTitle . showT $ p)

data LayoutProperty = TomeNumber | LineNumber | PageNumber
    deriving (Show, Eq, Enum)

instance RDFproperties LayoutProperty where
    mkRDFproperty p = RDFproperty $ layoutURItext <#> (toLowerStart . showT $ p)

--
----newtype ParaID = ParaID Int deriving (Show, Eq)
------ just to avoid confusions
----unparaID (ParaID t) = t
------instance Zeros ParaID where zero =  formatParaID zero
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

--inParaSigl :: TextState2 -> ParaNum -> ParaSigl
--inParaSigl  textstate pn = ParaSigl $ (extendSlashRDFsubj
--          (formatParaID .unparaNum $ pn)
--        (  buchURIx $ textstate))
-- ^ convert the inpart id into an uri

debugTurtle = True

convOneTZ2triple :: TextState2 -> TZ -> [Triple]
-- produce all triples necessary for each line item
convOneTZ2triple textstate tz  = case tz of
--    TZzahl {}  -> errorT ["formParagraphs","should not have TZzahl left", showT tz]
    TZmarkup {} -> lineTriple textstate tz
    TZleer {} -> [] -- where not elliminated?
------         errorT ["formParagraphs","should not leer", showT tz]
    TZtext {} -> lineTriple textstate tz

--otherTriple :: TextState2 -> TZ2 -> [Triple]
---- make triples for other markup (author etc.)
--otherTriple textstate tz =
--    [mkTripleLang (tz2lang tz) (unParaSigl sigl) (mkRDFproperty mk) (twm $ tz2text tz)]
--    where
--        sigl = paraSigl textstate . tz2para $ tz
--        mk = tz2tok tz

lineTriple :: TextState2 -> TZ  -> [Triple]
-- ^ enter a line triple
lineTriple textstate  tz =
    [ mkTripleRef (unLineSigl sigl) (mkRDFproperty LineNumber) (buchURIx textstate)
    , mkTripleType (unLineSigl sigl) (mkRDFtype Line)
    , mkTriplePartOf (unLineSigl sigl)   (buchURIx textstate)
    , mkTriplePartOf (unLineSigl sigl)   (unPageSigl pSigl)
        ]
    where
        sigl = lineSigl textstate .  tlline . tzloc $ tz
        pSigl = pageSigl textstate . tlpage . tzloc $ tz

--startSeiteTriple :: ParaSigl -> TZ2 -> [Triple]
---- ^ the triple for the page on which a paragraph starts
--startSeiteTriple sigl tz =  if null' seite then []
--            else [mkTripleText (unParaSigl sigl) (mkRDFproperty AufSeite)  seite]
--        where
--                seite = tlpage . tz2loc $ tz
--
--inBuchTriple :: TextState2 -> RDFsubj -> Triple
--inBuchTriple textstate sigl =   mkTripleRef sigl  (mkRDFproperty InBuch)  (buchURIx textstate)
---- probably not needed
--
--hlTriple :: TextState2 -> BuchToken -> TZ2 -> [Triple]
---- ^ produce the triples for header levels
--hlTriple textstate mk tz =
--    [ mkTripleLang lang (unParaSigl sigl) (mkRDFproperty mk)
--        (twm $ tz2text tz)
--    , inBuchTriple textstate (unParaSigl sigl)
--    , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart)
--                (unParaSigl inSigl)
--    , mkTripleType (unParaSigl sigl) (mkRDFtype mk)
--    ]
--    ++  startSeiteTriple sigl tz
--
--    where
--        sigl = paraSigl textstate . tz2para $  tz
--        inSigl = paraSigl textstate . tz2inPart $ tz
--        lang = tz2lang tz
--
--
--paraTriple :: TextState2 -> TZ2 -> [Triple]
---- | the triples to describe a paragraph, text as BuchParagraph, inPart
---- todo
---- with the filename given by the sigl
--paraTriple textstate tz =
--    [
----    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ((decodeLatin1 . encodeUtf8 )  $ zeilenText tz)
--    mkTripleLang lang (unParaSigl sigl) (mkRDFproperty BuchParagraph)
--                                        (zeilenText tz)
----    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ( zeilenText tz)
--                    -- do not remove hyphens and text breaks, exactly as in buch
--                    -- was BuchParagraphLayout
--    , inBuchTriple textstate (unParaSigl sigl)
--    , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart)
--                        (unParaSigl inSigl)
--    , mkTripleType (unParaSigl sigl) (mkRDFtype BuchParagraph)]
--     ++ startSeiteTriple sigl tz
--     -- page is text, not a number?
--
--    where
--        sigl = paraSigl textstate . tz2para $  tz
--        inSigl = paraSigl textstate . tz2inPart $ tz
--        lang = tz2lang tz

test_1BAE_J = testVar3File result1A "resultBAE1" "resultJ1" produceLayoutTriples
test_2BAE_J = testVar3File result2A "resultBAE2" "resultJ2" produceLayoutTriples
test_3BAE_J = testVar3File result3A "resultBAE3" "resultJ3" produceLayoutTriples
test_4BAE_J = testVar3File result4A "resultBAE4" "resultJ4" produceLayoutTriples
test_5BAE_J = testVar3File result5A "resultBAE5" "resultJ5" produceLayoutTriples
test_6BAE_J = testVar3File result6A "resultBAE6" "resultJ6" produceLayoutTriples
--test_7BAE_J = testVar3File result7A "resultBAE7" "resultJ7" produceLayoutTriples
test_8BAE_J = testVar3File result8A "resultBAE8" "resultJ8" produceLayoutTriples




