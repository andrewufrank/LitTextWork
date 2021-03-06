-- | extension for triple making and similar
-- but not the codes for the relations - these go to the specific
-- construction programs, to link directly with the definitions
-- and not the prefix stuff, which goes to prefs
--
-- the URI are always open at end and connecting must add separator
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns  #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.RDF.Extension (
    module Data.RDF.Extension
    , module Data.RDF
    , module Data.RDF.Prefs
--    , module Uniform.Convenience.LitTypes
    , (</>)
    )     where


import           Test.Framework
import Text.Printf
import           Data.Map            as Map (fromList)
import           Data.RDF            (Node, Triple (..), lnode, objectOf,
                                      plainL, plainLL, triple, typedL, unode)
import           Data.RDF            as RDF
import qualified Data.RDF            as RDF
import           Data.RDF.Prefs
import qualified Data.RDF.Types      as RDF (RDF (..), RdfSerializer (..))
-- import           Data.Text           hiding (map)
-- import qualified Data.Text           as T (append, concat, null, strip)
import           Uniform.Error
-- import           Uniform.FileIO      (thd3)
-- import           Uniform.StringInfix ((</>))
import           Uniform.Strings
import           Uniform.Zero
----import           Uniform.Convenience.LitTypes
type PartURI = Text
-- ^ TOOD should be used wherever a Text string is a URI code
-- there is another type...

--gerastreeURI =    "http://gerastree.at"  :: PartURI
-- the base url - with no closing
--baseurl = Just gerastreeURI :: Maybe Text

newtype RDFproperty = RDFproperty Text deriving (Show, Eq)
-- ^ a type to identify the RDFproperties
unRDFproperty (RDFproperty a) = a

class RDFproperties p where
    mkRDFproperty :: p -> RDFproperty
    -- TODO change to lowercase initial character

-- class URIs a b where
--   extendURI :: a -> b

newtype RDFsubj = RDFsubj Text deriving (Show, Eq)
-- ^ a type to identify the RDF subject value
unRDFsubj (RDFsubj a) = a

extendHashRDFsubj ::  Text -> RDFsubj ->  RDFsubj
extendHashRDFsubj e a = RDFsubj .  (<#> e) . unRDFsubj $ a
extendSlashRDFsubj :: Text -> RDFsubj  -> RDFsubj
extendSlashRDFsubj e a = RDFsubj .  (</> e) . unRDFsubj $ a
-- makeRDFproperty :: PartURI -> Text -> RDFproperty
-- makeRDFproperty a e = RDFproperty $ a <#> e
-- makeRDFtype :: PartURI -> Text -> RDFtype
-- makeRDFtype a e = RDFtype $ a <#> e

newtype RDFtype = RDFtype Text deriving (Show, Eq)
-- ^ the types for the rdf type values
unRDFtype (RDFtype a) = a

class RDFtypes p where
    mkRDFtype :: p -> RDFtype


data LanguageCode = NoLanguage | German | USenglish | English
    | French | Spanish | Italian   deriving (Eq, Ord, Show, Read)
instance Zeros LanguageCode where zero = NoLanguage

parseLanguageCode :: Text -> LanguageCode
parseLanguageCode "de" = German
parseLanguageCode "deu" = German
parseLanguageCode "en" = English
parseLanguageCode "fr" = French
parseLanguageCode "fre" = French
parseLanguageCode "sp" = Spanish
parseLanguageCode "spa" = Spanish   -- correct?
parseLanguageCode "it" = Italian
parseLanguageCode "ita" = Italian  -- correct?
parseLanguageCode "xx" = NoLanguage
parseLanguageCode "xxx" = NoLanguage
parseLanguageCode c = errorT ["Extension.hs = parseLanguageCode ", c, "not found"]

giveCode :: LanguageCode -> Text
-- produce the 2 character language code w3c
giveCode NoLanguage = "xx"  -- error "giveCode for zero - nolanguage"
giveCode German     = "de"
giveCode USenglish  = "us"
giveCode English    = "en"
giveCode French    = "fr"
giveCode Spanish    = "sp"
giveCode Italian    = "it"
giveCode s          = error ("giveCode 2chars to" ++ show s)

giveCode3 :: LanguageCode -> Text
-- ^ produce the three character language code for wordnet
giveCode3 NoLanguage = "xxx" -- error "giveCode3 for zero - nolanguage"
giveCode3 German     = "deu"
-- giveCode3 USenglish  = "us"
giveCode3 English    = "eng"
giveCode3 French    = "fre"
giveCode3 Spanish    = "spa"
giveCode3 Italian    = "ita"
giveCode3 s          = error ("giveCode 3 chars to" ++ show s)

readLanguageCode :: Text -> Text -> LanguageCode
-- ^ read the code for the language German, Deutsch, Englisch
--readLanguageCode  = readNoteT
-- todo change for not case sen
readLanguageCode _ "Deutsch" = German
readLanguageCode _ "deutsch" = German
readLanguageCode _ "german" = German
readLanguageCode _ "english" = English
readLanguageCode _ "Englisch" = English
readLanguageCode _ "french" = French
readLanguageCode _ "spanish" = Spanish
readLanguageCode _ "italian" = Italian
readLanguageCode msg l  = readNoteT msg l

getTripleLanguage :: Triple -> LanguageCode
-- find the language code in the triple (2 or 3 char)
getTripleLanguage = getLangCode . objectOf

ex1 = Triple (UNode "http://gerastree.at/kurz#kurz-005")
    (UNode "http://gerastree.at/lit_2014#BuchParagraph")
    (LNode (PlainLL "und mehr text in deutsch. test2 erfuellt?" "de"))

t1 = getTripleLanguage ex1
s1 = RDFsubj "http://gerastree.at/kurz#kurz-005"
r1 = RDFproperty "testint"

getLangCode :: Node -> LanguageCode
getLangCode (RDF.LNode (RDF.PlainLL _ l)) = parseLanguageCode l
getLangCode _ = errorT [" RDF4extension, extension.hs"
        , "indicates that getLangCode is called for non language coded obj", "NoLanguage"]

--splitTriple :: Triple -> (RDF.Node, Node, Node)
---- split the triple in three parts
---- inverse of curried triple (make) operation
--splitTriple (Triple s p o) = (s,p,o)

-- mkTripleDE :: Text -> RDFproperty -> Text -> [Triple]
-- -- | make a single triple in a list (german)
-- mkTripleDE s p o = [mkTripleDE1 s (unRDFproperty p) o]

mkTripleText ::    RDFsubj -> RDFproperty -> Text ->  Triple
-- | make a single triple without language Code
mkTripleText   s p o = triple subj pred obj
        where
            subj = unode . unRDFsubj $ s
            pred = unode (unRDFproperty p)
            obj = lnode (plainL  o )  -- fuer  text symbole

mkTripleLang :: LanguageCode ->  RDFsubj -> RDFproperty -> Text ->  Triple
-- | make a single triple with a language Code
mkTripleLang lg s p o = triple subj pred obj
        where
            subj = unode . unRDFsubj $ s
            pred = unode (unRDFproperty p)
            obj = lnode (plainLL (  o) ( giveCode lg))  -- fuer sprachorientierten text

mkTripleLang3 :: LanguageCode ->  RDFsubj -> RDFproperty -> Text ->  Triple
-- | make a single triple with a language Code using the 3 char codes from wordnet
mkTripleLang3 lg s p o = triple subj pred obj
        where
            subj = unode . unRDFsubj $ s
            pred = unode (unRDFproperty p)
            obj = lnode (plainLL (  o) ( giveCode3 lg))  -- fuer sprachorientierten text

mkTripleDE1 ::    RDFsubj -> RDFproperty -> Text ->  Triple
-- | make a triple for a german text
mkTripleDE1 s p o = triple subj pred obj
        where
            subj = unode . unRDFsubj $ s
            pred = unode (unRDFproperty p)
            obj = lnode (plainLL (  o) ( "de"))  -- fuer deutschen text

mkTripleRef ::    RDFsubj -> RDFproperty -> RDFsubj ->  Triple
mkTripleRef s p o = triple subj pred obj
    where
        subj = unode . unRDFsubj $ s
        pred = unode (unRDFproperty p)
        obj = unode . unRDFsubj $ o

integerUri =  "http://www.w3.org/2001/XMLSchema#integer"
decimalUri =  "http://www.w3.org/2001/XMLSchema#decimal"
doubleUri  =  "http://www.w3.org/2001/XMLSchema#double"

mkTripleInt :: RDFsubj -> RDFproperty -> Int -> Triple
-- ^ a triple with an int value
mkTripleInt s p o = mkTripleInteger s p (toInteger o)

mkTripleInteger ::    RDFsubj -> RDFproperty -> Integer ->  Triple
-- ^ a triple with an integer value
mkTripleInteger s p o = triple subj pred obj
    where
        subj = unode . unRDFsubj $ s
        pred = unode (unRDFproperty p)
        obj = lnode (TypedL (s2t . printf  ['%', '0','1','d'] $ o) integerUri)

zo :: Integer -> Triple
zo n =
    Triple (UNode "http://gerastree.at/kurz#kurz-005")
      (UNode "testint")
      (LNode (TypedL (showT n) integerUri))

-- to demonstrate error i rdf4h
--test_typed = assertEqual (TypedL "0" integerUri)
--            (typedL "0" integerUri)

test_typed0 = assertEqual (zo 0)
            (mkTripleInteger s1 r1 0 )
test_typed100 = assertEqual (zo 100)
            (mkTripleInteger s1 r1 100 )
test_typedneg1 = assertEqual (zo (-1))
            (mkTripleInteger s1 r1 (-1) )
test_typedneg20 = assertEqual (zo (-20))
            (mkTripleInteger s1 r1 (-20) )

rdfsURI =  "http://www.w3.org/2000/01/rdf-schema#"
rdfURI =  "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

mkTripleType :: RDFsubj -> RDFtype -> Triple
-- ^ make a triple with the rdf type = a
mkTripleType s  o = triple subj pred obj
    where
        subj = unode . unRDFsubj $ s
        pred = unode $ rdfURI <> "type"
        obj = unode . unRDFtype $ o

mkTriplePartOf :: RDFsubj -> RDFsubj -> Triple
-- ^ make a triple with the rdfs partOf predicate
mkTriplePartOf s  o = triple subj pred obj
    where
        subj = unode . unRDFsubj $ s
        pred = unode $ rdfsURI <> "partOf"
        obj = unode . unRDFsubj $ o
