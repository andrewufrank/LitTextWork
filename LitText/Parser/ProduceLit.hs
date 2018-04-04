 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceText
-- Copyright   :  andrew u frank -
--
-- | producing the triples for representing the literary text
-- mostly avoided in v2 which does only include what is actually used to classify texts
-- especially fables

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Parser.ProduceLit (module Parser.ProduceLit
        , Text, Triple, Path (..), Abs, Rel, Dir, File
        , module LitTypes.TextDescriptor
    ) where

import           Data.Char               (toLower)
import           Data.Maybe               (isNothing)
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import Uniform.Strings ((</>))  -- for PartURI
-- import Uniform.HttpURI (uriT)
import           Text.Printf         (printf)
import           Uniform.Error      --     (errorT)
import LitTypes.TextDescriptor hiding ((</>)) -- from Foundation
import BuchCode.BuchToken hiding ((</>), (<.>))

--litURItext =  PartURI ((unPartURI  rdfBase)  </> "lit_2014") :: PartURI
litURItext = append2partURI rdfBase "/lit_2014"
dcURItext = PartURI "http://purl.org/dc/elements/1.1" :: PartURI
-- no terminating /

buchURIx textstate = RDFsubj $ (unPartURI rdfBase)
            <#> authorDir textstate <-> buchName textstate
-- id of buch, part and sentence or page is attached

type ParaID = Int   -- should be typed?

newtype ParaSigl = ParaSigl RDFsubj deriving (Show, Read, Eq, Ord)
unParaSigl (ParaSigl t) = t

formatParaID :: ParaID -> Text
formatParaID nr =   "P" <> (s2t . printf  ('%' : '0' : '5' : 'd' :[]) $  nr )

paraSigl :: TextDescriptor -> ParaNum -> ParaSigl
paraSigl textstate pn = ParaSigl ( extendSlashRDFsubj
                (formatParaID . unparaNum $ pn)
                      ( buchURIx $ textstate)
                      )

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
    mkRDFproperty p = RDFproperty $ unPartURI litURItext  <#> (toLowerStart . showT $ p)
instance RDFproperties BuchToken where
    mkRDFproperty tk =
        case tk of
            BuchAuthor -> RDFproperty $ unPartURI dcURItext </> "creator"
            BuchVerlag -> RDFproperty $ unPartURI dcURItext </> "publisher"
            BuchSprache -> RDFproperty $ unPartURI dcURItext </>  "language"
            BuchTitel -> RDFproperty $ unPartURI dcURItext </> "title"
            BuchOriginalFile -> RDFproperty $ unPartURI dcURItext </> "source"
            otherwise -> RDFproperty $  unPartURI litURItext <#> (toLowerStart . markerPure $ tk)

-- toLowerStart :: Text -> Text
-- -- convert the first character to lowercase
-- toLowerStart t = (toLower . T.head $ t ) `cons` (T.tail t)

instance RDFtypes Text where
    mkRDFtype p =  RDFtype $ unPartURI litURItext <#> (toTitle p)
instance RDFtypes RDFtype where
    mkRDFtype p =  RDFtype $ unPartURI litURItext <#> (toTitle . showT $ p)
instance RDFtypes BuchToken where
    mkRDFtype tk = RDFtype $ unPartURI litURItext <#> (toTitle . markerPure $ tk)



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

