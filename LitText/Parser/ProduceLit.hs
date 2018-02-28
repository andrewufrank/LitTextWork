 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceText
-- Copyright   :  andrew u frank -
--
-- | producing the triples for representing the literary text
-- mostly avoided in v2 which does only include what is actually used to classify texts
-- especially fables

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Parser.ProduceLit (module Parser.ProduceLit
        , Text, Triple, Path (..), Abs, Rel, Dir, File
        , module Data.RDF.FileTypes
    ) where

--import           Test.Framework
import           Data.Char               (toLower)
import           Data.Maybe               (isNothing)
import           Data.RDF
import           Data.RDF.Extension
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import Uniform.Strings ((</>))  -- for PartURI
import Uniform.HttpURI (uriT)
--import Parser.ReadMarkupAB
--import Parser.ProduceLayout
--import BuchCode.BuchToken
--import BuchCode.MarkupText
--import Lines2para.Lines2para -- hiding ((</>))
--import Lines2para.HandleLayout
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
--import Uniform.TestHarness
import Data.RDF.FileTypes hiding ((</>), (<.>))
import Parser.TextDescriptor hiding ((</>)) -- from Foundation
import BuchCode.BuchToken hiding ((</>), (<.>))
import Producer.Servers  (rdfBase)  -- for test
--import Parser.ProduceLayout (buchURIx)
import Parser.NLPvocabulary hiding ((</>), (<.>))

litURItext =   (uriT rdfBase) </> "lit_2014" :: PartURI
dcURItext = "http://purl.org/dc/elements/1.1" :: PartURI
-- no terminating /

produceLitTriples ::  TextDescriptor -> [TZ2] -> [Triple]  -- test C=BAE -> H
-- convert a text to the triples under lit: main function for the conversion
produceLitTriples textstate tz2s = (werkTriple textstate)
                    ++ (concatMap (convOneTextZeile2triple textstate) tz2s)
--                       ++ ( if includeText textstate
--                                then (concatMap (convOneTextZeile2triple textstate) tz2s)
--                                else []
--                           )
-- the include is later in the convOneText..


werkTriple :: TextDescriptor ->   [Triple]
-- ^ produce a werk, has the properties in markup
werkTriple textstate   =
    [ mkTripleType buchUri (mkRDFtype ("Werk" :: Text))
    , mkTripleText buchUri (mkRDFproperty Version) "4"      -- change for disruptive changes
--            3 was for gerastree prefixes, 4 is using dublin core
--              and inlcudes sentenceForm again
    , mkTripleText buchUri (mkRDFproperty MinorVersion) "0"  -- bump for any change in the RDF vocabulary
    ]
    where
        buchUri = buchURIx textstate


data LitProperty =  HasTitle | InWerk | InBuch | InPart
        | AufSeite  -- ^ text starts on this page
        | Text -- instead of  Titel | HL1 | HL2 | HL3 | Paragraph
        | Version | MinorVersion  -- ^ the version of the lit ontology
        --  the text for this textual unit
        deriving (Show, Eq, Enum)

instance RDFproperties LitProperty where
    mkRDFproperty p = RDFproperty $  litURItext <#> (toLowerStart . showT $ p)
instance RDFproperties BuchToken where
    mkRDFproperty tk =
        case tk of
            BuchAuthor -> RDFproperty $ dcURItext </> "creator"
            BuchVerlag -> RDFproperty $ dcURItext </> "publisher"
            BuchSprache -> RDFproperty $ dcURItext </>  "language"
            BuchTitel -> RDFproperty $ dcURItext </> "title"
            BuchOriginalFile -> RDFproperty $ dcURItext </> "source"
            otherwise -> RDFproperty $  litURItext <#> (toLowerStart . markerPure $ tk)

-- toLowerStart :: Text -> Text
-- -- convert the first character to lowercase
-- toLowerStart t = (toLower . T.head $ t ) `cons` (T.tail t)

instance RDFtypes Text where
    mkRDFtype p =  RDFtype $ litURItext <#> (toTitle p)
instance RDFtypes RDFtype where
    mkRDFtype p =  RDFtype $ litURItext <#> (toTitle . showT $ p)
instance RDFtypes BuchToken where
    mkRDFtype tk = RDFtype $ litURItext <#> (toTitle . markerPure $ tk)



convOneTextZeile2triple :: TextDescriptor -> TZ2 -> [Triple]
-- produce all triples necessary for each line item
-- not executed when text not included
convOneTextZeile2triple textstate tz  = case tz of
    TZ2markup {} -> hlTriple textstate  tz
                      ++ (if tz2tok tz `elem` [BuchKlappenText .. BuchTitel]
                            then  otherBuchTriple textstate tz
                            else []
                            )


    TZ2para {} -> if includeText textstate
                        then error "tztpera adfasd" -- paraTriple textstate tz
                        else []

otherBuchTriple :: TextDescriptor -> TZ2 -> [Triple]
-- make triples for other markup (author etc.), which apply to the buch as a whole
otherBuchTriple textstate tz =
--    [mkTripleLang3 (tz2lang tz) buchUri (mkRDFproperty mk)
--                     (twm $ tz2text tz)]
    [mkTripleLang33   buchUri (mkRDFproperty mk)
                     (twm1 $ tz2text tz)]
    where
            buchUri = buchURIx textstate
--        sigl = paraSigl textstate . tz2para $ tz
            mk = tz2tok tz


inWerkTriple :: TextDescriptor -> RDFsubj -> Triple
inWerkTriple textstate sigl =   mkTripleRef sigl  (mkRDFproperty InWerk)
                                    (buchURIx textstate)
-- probably not needed ??

hlTriple :: TextDescriptor ->   TZ2 -> [Triple]
-- ^ produce the triples for header levels
hlTriple textstate  tz =   if includeText textstate
        then
            [ mkTripleLang33  (unParaSigl sigl) (mkRDFproperty Text)
                (twm1 $ tz2text tz)
--             mkTripleLang3  (unParaSigl sigl) (mkRDFproperty Text)
--                (twm $ tz2text tz)
            , inWerkTriple textstate (unParaSigl sigl)
            , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart)
                        (unParaSigl inSigl)
            , mkTripleType (unParaSigl sigl) (mkRDFtype mk)
            ]
--      ++  startSeiteTriple sigl tz
        else []

    where
        sigl = paraSigl textstate . tz2para $  tz
        inSigl = paraSigl textstate . tz2inPart $ tz
--        lang = tz2lang tz
        mk = tz2tok tz


--

