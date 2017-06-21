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


module Parser.ProduceLit (module Parser.ProduceLit
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


litURItext =   gerastreeURI </> "lit_2014" :: PartURI

produceLitTriples ::  TextState2 -> [TZ2] -> [Triple]  -- test C=BAE -> H
-- convert a text to the triples under lit: main entry point
produceLitTriples textstate = concatMap (convOneTextZeile2triple textstate)


buchURIx textstate = RDFsubj $ gerastreeURI
            <#> authorText textstate <-> buchnameText textstate
-- id of buch, part and sentence or page is attached

data LitProperty = IsBuch | HasTitle | InBuch | InPart
        | AufSeite  -- ^ text starts on this page
        -- | Titel | HL1 | HL2 | HL3 | Paragraph  -- ^ the text for this textual unit
        deriving (Show, Eq, Enum)

instance RDFproperties LitProperty where
    mkRDFproperty p = RDFproperty $  litURItext <#> (toLowerStart . showT $ p)
instance RDFproperties BuchToken where
    mkRDFproperty t = RDFproperty $  litURItext <#> (toLowerStart . markerPure $ t)

-- toLowerStart :: Text -> Text
-- -- convert the first character to lowercase
-- toLowerStart t = (toLower . T.head $ t ) `cons` (T.tail t)

instance RDFtypes RDFtype where
    mkRDFtype p =  RDFtype $ litURItext <#> (toTitle . showT $ p)
instance RDFtypes BuchToken where
    mkRDFtype tk = RDFtype $ litURItext <#> (toTitle . markerPure $ tk)

--newtype ParaID = ParaID Int deriving (Show, Eq)
---- just to avoid confusions
--unparaID (ParaID t) = t
----instance Zeros ParaID where zero =  formatParaID zero

newtype ParaSigl = ParaSigl RDFsubj
unParaSigl (ParaSigl t) = t

formatParaID :: Int -> Text
formatParaID nr =   "P" <> (s2t . printf  ('%' : '0' : '5' : 'd' :[]) $  nr )
-- format to 5 digits
--
--formatLineID :: Int -> Text
--formatLineID nr = "L" <> (s2t . printf  ('%' : '0' : '3' : 'd' :[]) $  nr )
---- format to 3 digits

paraSigl :: TextState2 -> ParaNum -> ParaSigl
paraSigl textstate pn = ParaSigl ( extendSlashRDFsubj
                (formatParaID . unparaNum $ pn)
                      ( buchURIx $ textstate)
                      )

--inParaSigl :: TextState2 -> ParaNum -> ParaSigl
--inParaSigl  textstate pn = ParaSigl $ (extendSlashRDFsubj
--          (formatParaID .unparaNum $ pn)
--        (  buchURIx $ textstate))
-- ^ convert the inpart id into an uri

debugTurtle = True

convOneTextZeile2triple :: TextState2 -> TZ2 -> [Triple]
-- produce all triples necessary for each line item
convOneTextZeile2triple textstate tz  = case tz of
--    TZzahl {}  -> errorT ["formParagraphs","should not have TZzahl left", showT tz]
    TZ2markup {} -> case tz2tok tz of
            BuchTitel -> titleTriple textstate tz
            BuchHL1   -> hlTriple textstate BuchHL1 tz
            BuchHL2   -> hlTriple textstate BuchHL2 tz
            BuchHL3   -> hlTriple textstate BuchHL3 tz
            _         -> otherTriple textstate tz
--    TZleer {} -> [] -- where not elliminated?
----         errorT ["formParagraphs","should not leer", showT tz]
--    TZtext {} -> errorT ["formParagraphs","should not have single text", showT tz]
--    TZkurz {} -> errorT ["formParagraphs","should not have single kurz", showT tz]
    TZ2para {} -> paraTriple textstate tz

otherTriple :: TextState2 -> TZ2 -> [Triple]
-- make triples for other markup (author etc.)
otherTriple textstate tz =
    [mkTripleLang (tz2lang tz) (unParaSigl sigl) (mkRDFproperty mk) (twm $ tz2text tz)]
    where
        sigl = paraSigl textstate . tz2para $ tz
        mk = tz2tok tz

titleTriple :: TextState2 -> TZ2 -> [Triple]
-- ^ initialize a text with a title
-- linked
titleTriple textstate  tz =
    [mkTripleLang (tz2lang tz)  (unParaSigl sigl)
                    (mkRDFproperty BuchTitel) (twm $ tz2text tz)
    , mkTripleRef (unParaSigl sigl) (mkRDFproperty IsBuch) (buchURIx textstate)
    , mkTripleType (unParaSigl sigl) (mkRDFtype BuchTitel)
    ]
    where
        sigl = paraSigl textstate . tz2para $ tz

startSeiteTriple :: ParaSigl -> TZ2 -> [Triple]
-- ^ the triple for the page on which a paragraph starts
startSeiteTriple sigl tz =  if null' seite then []
            else [mkTripleText (unParaSigl sigl) (mkRDFproperty AufSeite)  seite]
        where
                seite = tlpage . tz2loc $ tz

inBuchTriple :: TextState2 -> RDFsubj -> Triple
inBuchTriple textstate sigl =   mkTripleRef sigl  (mkRDFproperty InBuch)  (buchURIx textstate)
-- probably not needed

hlTriple :: TextState2 -> BuchToken -> TZ2 -> [Triple]
-- ^ produce the triples for header levels
hlTriple textstate mk tz =
    [ mkTripleLang lang (unParaSigl sigl) (mkRDFproperty mk)
        (twm $ tz2text tz)
    , inBuchTriple textstate (unParaSigl sigl)
    , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart)
                (unParaSigl inSigl)
    , mkTripleType (unParaSigl sigl) (mkRDFtype mk)
    ]
    ++  startSeiteTriple sigl tz

    where
        sigl = paraSigl textstate . tz2para $  tz
        inSigl = paraSigl textstate . tz2inPart $ tz
        lang = tz2lang tz


paraTriple :: TextState2 -> TZ2 -> [Triple]
-- | the triples to describe a paragraph, text as BuchParagraph, inPart
-- todo
-- with the filename given by the sigl
paraTriple textstate tz =
    [
--    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ((decodeLatin1 . encodeUtf8 )  $ zeilenText tz)
    mkTripleLang lang (unParaSigl sigl) (mkRDFproperty BuchParagraph)
                                        (zeilenText tz)
--    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ( zeilenText tz)
                    -- do not remove hyphens and text breaks, exactly as in buch
                    -- was BuchParagraphLayout
    , inBuchTriple textstate (unParaSigl sigl)
    , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart)
                        (unParaSigl inSigl)
    , mkTripleType (unParaSigl sigl) (mkRDFtype BuchParagraph)]
     ++ startSeiteTriple sigl tz
     -- page is text, not a number?

    where
        sigl = paraSigl textstate . tz2para $  tz
        inSigl = paraSigl textstate . tz2inPart $ tz
        lang = tz2lang tz

test_1BAE_H = testVar3File result1A "resultBAE1" "resultH1" produceLitTriples
test_2BAE_H = testVar3File result2A "resultBAE2" "resultH2" produceLitTriples
test_3BAE_H = testVar3File result3A "resultBAE3" "resultH3" produceLitTriples
test_4BAE_H = testVar3File result4A "resultBAE4" "resultH4" produceLitTriples
test_5BAE_H = testVar3File result5A "resultBAE5" "resultH5" produceLitTriples
test_6BAE_H = testVar3File result6A "resultBAE6" "resultH6" produceLitTriples
--test_7BAE_H = testVar3File result7A "resultBAE7" "resultH7" produceLitTriples
test_8BAE_H = testVar3File result8A "resultBAE8" "resultH8" produceLitTriples




