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


module Parser.ProduceLit
    (produceLitTriples
    , gerastreeURI
    -- , paraSigl
    , buchURIx
    , TextState2 (..)
    , LitProperty (..)
    , paraSigl, inParaSigl
    , htf_thisModulesTests
    , result1H_tripleResult1
--    , textstate0
--    , textstate0
--    , result1C_tzResult1
    ) where

import           Test.Framework
import           Data.Char               (toLower)
import           Data.RDF
import           Data.RDF.Extension
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import           Parser.Foundation
import Parser.ReadMarkupAB (result1A, result2A, result3A, result4A)
import Lines2para.Lines2para
import          Lines2para.Lines2paraTests (result1C, result2C, result3C
--        , result1B_textstate, result2B_textstate, result3B_textstate
--            , result1A_textstate, result2A_textstate,result3A_textstate
            )
import           Uniform.Error           (errorT)
import           Uniform.Strings         hiding ((<|>))
--import Uniform.FileIO (LegalPathname (..))  -- for test

litURItext =  gerastreeURI </> "lit_2014" :: PartURI

produceLitTriples ::  TextState2 -> [TZ] -> [Triple]  -- test C -> H
-- convert a text to the triples under lit: main entry point
produceLitTriples textstate = concatMap (convOneTextZeile2triple textstate)

test_1_C_H = assertEqual result1H_tripleResult1 (produceLitTriples result1A result1C)
test_2_C_H = assertEqual result2H_tripleResult1 (produceLitTriples result2A result2C)
test_3_C_H = assertEqual result3H_tripleResult1 (produceLitTriples result3A result3C)

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

paraSigl :: TextState2 -> TZ -> ParaSigl
paraSigl textstate tz = ParaSigl $ extendSlashRDFsubj  (unparaID . tlpara . tzloc $ tz)
                          (buchURIx textstate)

inParaSigl :: TextState2 -> TZ -> ParaSigl
inParaSigl  textstate tz = ParaSigl $ RDFsubj $ (unRDFsubj . buchURIx $ textstate)
        </> (unparaID . tzInPart $ tz)
-- ^ convert the inpart id into an uri

debugTurtle = True

convOneTextZeile2triple :: TextState2 -> TZ -> [Triple]
-- produce all triples necessary for each line item
convOneTextZeile2triple textstate tz  = case tz of
    TZzahl {}  -> errorT ["formParagraphs","should not have TZzahl left", showT tz]
    TZmarkup {} -> case tztok tz of
            BuchTitel -> titleTriple textstate tz
            BuchHL1   -> hlTriple textstate BuchHL1 tz
            BuchHL2   -> hlTriple textstate BuchHL2 tz
            BuchHL3   -> hlTriple textstate BuchHL3 tz
            _         -> otherTriple textstate tz
    TZleer {} -> [] -- where not elliminated?
--         errorT ["formParagraphs","should not leer", showT tz]
    TZtext {} -> errorT ["formParagraphs","should not have single text", showT tz]
    TZkurz {} -> errorT ["formParagraphs","should not have single kurz", showT tz]
    TZpara {} -> paraTriple textstate tz

otherTriple :: TextState2 -> TZ -> [Triple]
-- make triples for other markup (author etc.)
otherTriple textstate tz =
    [mkTripleLang (tzlang tz) (unParaSigl sigl) (mkRDFproperty mk) (tztext tz)]
    where
        sigl = paraSigl textstate tz
        mk = tztok tz

titleTriple :: TextState2 -> TZ -> [Triple]
-- ^ initialize a text with a title
-- linked
titleTriple textstate  tz =
    [mkTripleLang (tzlang tz)  (unParaSigl sigl) (mkRDFproperty BuchTitel) (tztext tz)
    , mkTripleRef (unParaSigl sigl) (mkRDFproperty IsBuch) (buchURIx textstate)
    , mkTripleType (unParaSigl sigl) (mkRDFtype BuchTitel)
    ]
    where
        sigl = paraSigl textstate tz

startSeiteTriple :: ParaSigl -> TZ -> [Triple]
-- ^ the triple for the page on which a paragraph starts
startSeiteTriple sigl tz =  if null' seite then []
                    else [mkTripleText (unParaSigl sigl) (mkRDFproperty AufSeite)  seite]
        where
                seite = tlpage . tzloc $ tz

inBuchTriple :: TextState2 -> RDFsubj -> Triple
inBuchTriple textstate sigl =   mkTripleRef sigl  (mkRDFproperty InBuch)  (buchURIx textstate)
-- probably not needed

hlTriple :: TextState2 -> BuchToken -> TZ -> [Triple]
-- ^ produce the triples for header levels
hlTriple textstate mk tz =
    [ mkTripleLang lang (unParaSigl sigl) (mkRDFproperty mk) (tztext tz)
    , inBuchTriple textstate (unParaSigl sigl)
    , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart) (unParaSigl $ inParaSigl textstate tz)
    , mkTripleType (unParaSigl sigl) (mkRDFtype mk)
    ]
    ++  startSeiteTriple sigl tz

    where
        sigl = paraSigl textstate tz
        lang = tzlang tz


paraTriple :: TextState2 -> TZ -> [Triple]
-- | the triples to describe a paragraph, text as BuchParagraph, inPart
-- todo
-- with the filename given by the sigl
paraTriple textstate tz =
    [
--    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ((decodeLatin1 . encodeUtf8 )  $ zeilenText tz)
    mkTripleLang lang (unParaSigl sigl) (mkRDFproperty BuchParagraph) (  zeilenText tz)
--    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ( zeilenText tz)
                    -- do not remove hyphens and text breaks, exactly as in buch
                    -- was BuchParagraphLayout
    , inBuchTriple textstate (unParaSigl sigl)
    , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart)  (unParaSigl $ inParaSigl textstate tz)
    , mkTripleType (unParaSigl sigl) (mkRDFtype BuchParagraph)]
     ++ startSeiteTriple sigl tz
     -- page is text, not a number?

    where
        sigl = paraSigl textstate tz
        lang = tzlang tz

test_1_C_H_TZ_litTriples_1 =   do
--    putIOwords ["test_ProduceLit: read text for "] -- tzResult]
    let t1 = produceLitTriples result1A result1C
--    putIOwords ["test_ProduceLit: result (for next) ", s2t $ show t1]
--    putIOwords ["test_parseToTZ:  result ", show' t1]
    assertEqual result1H_tripleResult1 t1

test_2_C_H_TZ_litTriples_1 =   do
--    putIOwords ["test_ProduceLit: read text for "] -- tzResult]
    let t1 = produceLitTriples result2A result2C
    assertEqual result2H_tripleResult1 t1

test_3_C_H_TZ_litTriples_1 =   do
--    putIOwords ["test_ProduceLit: read text for "] -- tzResult]
    let t1 = produceLitTriples result3A  result3C
    assertEqual result3H_tripleResult1 t1

#include "ProduceLit.res"


