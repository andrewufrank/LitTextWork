 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceText
-- Copyright   :  andrew u frank -
--
-- | producing the triples for representing the literary text
-- mostly avoided in v2 which does only include what is actually used to classify texts
-- especially fables

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
import           Data.Maybe               (isNothing)
import           Data.RDF
import           Data.RDF.Extension
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB
import Parser.ProduceLayout
import BuchCode.BuchToken
import BuchCode.MarkupText
import Lines2para.Lines2para -- hiding ((</>))
import Lines2para.HandleLayout
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
import Uniform.TestHarness
import Data.RDF.FileTypes

litURItext =   gerastreeURI </> "lit_2014" :: PartURI

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
    , mkTripleText buchUri (mkRDFproperty Version) "V2"
    ]
    where
        buchUri = buchURIx textstate

test_werk = assertEqual (RDFtype "http://gerastree.at/lit_2014#Werk")
                (mkRDFtype ( "Werk"::Text ))


data LitProperty =  HasTitle | InWerk | InBuch | InPart
        | AufSeite  -- ^ text starts on this page
        | Text -- instead of  Titel | HL1 | HL2 | HL3 | Paragraph
        | Version  -- ^ the version of the lit ontology
        --  the text for this textual unit
        deriving (Show, Eq, Enum)

instance RDFproperties LitProperty where
    mkRDFproperty p = RDFproperty $  litURItext <#> (toLowerStart . showT $ p)
instance RDFproperties BuchToken where
    mkRDFproperty t = RDFproperty $  litURItext <#> (toLowerStart . markerPure $ t)

-- toLowerStart :: Text -> Text
-- -- convert the first character to lowercase
-- toLowerStart t = (toLower . T.head $ t ) `cons` (T.tail t)

instance RDFtypes Text where
    mkRDFtype p =  RDFtype $ litURItext <#> (toTitle p)
instance RDFtypes RDFtype where
    mkRDFtype p =  RDFtype $ litURItext <#> (toTitle . showT $ p)
instance RDFtypes BuchToken where
    mkRDFtype tk = RDFtype $ litURItext <#> (toTitle . markerPure $ tk)


newtype ParaSigl = ParaSigl RDFsubj
unParaSigl (ParaSigl t) = t

formatParaID :: Int -> Text
formatParaID nr =   "P" <> (s2t . printf  ('%' : '0' : '5' : 'd' :[]) $  nr )
-- format to 5 digits
--
--formatLineID :: Int -> Text
--formatLineID nr = "L" <> (s2t . printf  ('%' : '0' : '3' : 'd' :[]) $  nr )
---- format to 3 digits

paraSigl :: TextDescriptor -> ParaNum -> ParaSigl
paraSigl textstate pn = ParaSigl ( extendSlashRDFsubj
                (formatParaID . unparaNum $ pn)
                      ( buchURIx $ textstate)
                      )

convOneTextZeile2triple :: TextDescriptor -> TZ2 -> [Triple]
-- produce all triples necessary for each line item
-- not executed when text not included
convOneTextZeile2triple textstate tz  = case tz of
    TZ2markup {} -> hlTriple textstate (tz2tok tz) tz
                      ++ (if tz2tok tz `elem` [BuchKlappenText .. BuchIV3]
                            then  otherBuchTriple textstate tz
                            else []
                            )


--    TZ2markup {} -> case tz2tok tz of
--            BuchTitel -> otherBuchTriple textstate tz
--                            ++ hlTriple textstate BuchTitel tz
--                            -- create a simple title property for the werk
--            BuchHL1   -> hlTriple textstate BuchHL1 tz
--            BuchHL2   -> hlTriple textstate BuchHL2 tz
--            BuchHL3   -> hlTriple textstate BuchHL3 tz
--            BuchAuthor -> otherBuchTriple textstate tz
--                            ++ hlTriple textstate BuchAuthor tz
--            BuchAuthor -> otherBuchTriple textstate tz
--                            ++ hlTriple textstate BuchAuthor tz
--            val ->  []
----                        otherBuchTriple textstate tz
    TZ2para {} -> if includeText textstate
                        then error "tztpera adfasd" -- paraTriple textstate tz
                        else []

otherBuchTriple :: TextDescriptor -> TZ2 -> [Triple]
-- make triples for other markup (author etc.), which apply to the buch as a whole
otherBuchTriple textstate tz =
    [mkTripleLang (tz2lang tz) buchUri (mkRDFproperty mk)
                     (twm $ tz2text tz)]
    where
            buchUri = buchURIx textstate
--        sigl = paraSigl textstate . tz2para $ tz
            mk = tz2tok tz

--otherBuchTriple2 :: TextState2 -> TZ2 -> [Triple]

--startSeiteTriplete tz = tlpage . tz2loc $ tz

inWerkTriple :: TextDescriptor -> RDFsubj -> Triple
inWerkTriple textstate sigl =   mkTripleRef sigl  (mkRDFproperty InWerk)
                                    (buchURIx textstate)
-- probably not needed ??

hlTriple :: TextDescriptor -> BuchToken -> TZ2 -> [Triple]
-- ^ produce the triples for header levels
hlTriple textstate mk tz =   if includeText textstate
        then
            [ mkTripleLang lang (unParaSigl sigl) (mkRDFproperty Text)
                (twm $ tz2text tz)
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
        lang = tz2lang tz


--paraTriple :: TextDescriptor -> TZ2 -> [Triple]
---- | the triples to describe a paragraph, text as BuchParagraph, inPart
---- todo
---- with the filename given by the sigl
---- not used when text is not included
--paraTriple textstate tz = -- if True then error "paprtriple xxx" else
--    [
----    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ((decodeLatin1 . encodeUtf8 )  $ zeilenText tz)
--    mkTripleLang lang (unParaSigl sigl) (mkRDFproperty Text)
--                                        (zeilenText tz)
----    mkTripleLang lang sigl (litURI <> markerPure BuchParagraph) ( zeilenText tz)
--                    -- do not remove hyphens and text breaks, exactly as in buch
--                    -- was BuchParagraphLayout
--    , inWerkTriple textstate (unParaSigl sigl)
--    , mkTripleRef (unParaSigl sigl) (mkRDFproperty InPart)
--                        (unParaSigl inSigl)
--    , mkTripleType (unParaSigl sigl) (mkRDFtype BuchParagraph)]
--
----     ++ startSeiteTriple sigl tz  -- needed v2?
--     -- page is text, not a number?
--
--    where
--        sigl = paraSigl textstate . tz2para $  tz
--        inSigl = paraSigl textstate . tz2inPart $ tz
--        lang = tz2lang tz

test_1BAE_H = testVar3File result1A "resultBAE1" "resultH1" produceLitTriples
test_2BAE_H = testVar3File result2A "resultBAE2" "resultH2" produceLitTriples
test_3BAE_H = testVar3File result3A "resultBAE3" "resultH3" produceLitTriples
--test_4BAE_H = testVar3File result4A "resultBAE4" "resultH4" produceLitTriples
--test_5BAE_H = testVar3File result5A "resultBAE5" "resultH5" produceLitTriples
--test_6BAE_H = testVar3File result6A "resultBAE6" "resultH6" produceLitTriples
----test_7BAE_H = testVar3File result7A "resultBAE7" "resultH7" produceLitTriples
--test_8BAE_H = testVar3File result8A "resultBAE8" "resultH8" produceLitTriples
--test_9BAE_H = testVar3File result9A "resultBAE9" "resultH9" produceLitTriples
--test_10BAE_H = testVar3File result10A "resultBAE10" "resultH10" produceLitTriples


writeLitTriples :: FilePath -> FilePath ->  IO ()
writeLitTriples source dest   = do
    testDataDir <- getAppUserDataDir "LitTextTest" -- :: Path Abs Dir
    let source2 =  addFileName testDataDir    source :: Path Abs File
    let dest2 =  addFileName testDataDir     dest :: Path Abs File
    runErr $ do
        putIOwords ["writeLitTriples", "write the triples as nt"]
        tripstext <- readFile2 ( source2)
        let trips = readNote "writeLitTriples" tripstext :: [Triple]
        write6 dest2 ntFileTriples trips
    assertBool True
    return ()


test_1H_K = writeLitTriples   "resultH1" "resultK1"
test_2H_K = writeLitTriples   "resultH2" "resultK2"
test_3H_K = writeLitTriples   "resultH3" "resultK3"
--test_4H_K = writeLitTriples   "resultH4" "resultK4"
--test_5H_K = writeLitTriples   "resultH5" "resultK5"
--test_6H_K = writeLitTriples   "resultH6" "resultK6"
--test_7H_K = writeLitTriples   "resultH7" "resultK7"
--test_8H_K = writeLitTriples   "resultH8" "resultK8"
--test_9H_K = writeLitTriples   "resultH9" "resultK9"
--test_10H_K = writeLitTriples   "resultH10" "resultK10"


