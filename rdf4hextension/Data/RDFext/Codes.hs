-- | extension for triple making and similar
-- but not the codes for the relations - these go to the specific
-- construction programs, to link directly with the definitions
-- and not the prefix stuff, which goes to prefs
--
-- the URI are always open at end and connecting must add separator
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

--{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
--{-# OPTIONS_GHC -fno-warn-overlapping-patterns  #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric
    , DeriveAnyClass  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}




module Data.RDFext.Codes ( module Data.RDFext.Codes
    , Triple (..), Node (..), LValue (..)
    )     where


import Text.Printf
import           Data.Map            as Map (fromList)
import           Data.RDF            (Node, Triple (..), lnode, objectOf,
                                      plainL, plainLL, triple, typedL, unode)
import           Data.RDF            as RDF
import qualified Data.RDF            as RDF


import qualified Data.RDF.Types      as RDF (RDF (..), RdfSerializer (..))
import           Uniform.Error
import           Uniform.Strings ((</>))
import           Uniform.Zero
import  GHC.Generics
import Uniform.Http

newtype IRI = IRI Text
    deriving (Show, Read, Eq, Ord, Generic, Zeros)
-- ^ a type for an IRI in the RDF setting (not used for server URI)
-- could be tested for validity
mkIRI = IRI
unIRI (IRI t) = t   -- do not export!
        -- necessary to use graph names for file names etc.

--mkHttPathFromIRI = mkHttpPath . unIRI

class IRIs i where
    append2IRI :: i -> Text -> i
    append2IRIwithSlash :: i -> Text -> i

instance IRIs IRI where
    append2IRI u1 t = IRI $ unIRI u1  <> t
    append2IRIwithSlash u1 t = IRI $ unIRI u1  </> t

instance IRIs RDFsubj where
    append2IRI u1 t = mkRDFsubj $ append2IRI (toIRI u1) t
    append2IRIwithSlash u1 t = mkRDFsubj $ append2IRIwithSlash (toIRI u1) t

newtype GraphName = GraphName Text
-- ^ the name for a graph
    deriving (Show, Read, Eq, Ord, Generic, Zeros)
mkGraphName = GraphName

instance RDFtypes GraphName where
    toIRI (GraphName s) = IRI s

newtype RDFdataset = RDFdataset Text
-- ^ the name for a dataset in a sparql endpoint
    deriving (Show, Read, Eq, Ord, Generic, Zeros)
mkRDFdataset = RDFdataset

instance RDFtypes RDFdataset where
    toIRI (RDFdataset s) = IRI s


newtype PartURI = PartURI Text
    deriving (Show, Read, Eq, Ord, Generic, Zeros)
-- unPartURI (PartURI t) = t
-- --instance Zeros PartURI where zero = PartURI zero

-- -- ^ TOOD should be used wherever a Text string is a URI code
-- -- there is another type - Network.URI, which is checked (and problems with Read class)
-- -- conversion from URI to text use uriT (not showT)
-- append2partURI :: PartURI -> Text -> PartURI
-- append2partURI u1 t = PartURI $  unPartURI u1  <> t

--gerastreeURI =    "http://gerastree.at"  :: PartURI
-- the base url - with no closing
--baseurl = Just gerastreeURI :: Maybe Text

newtype RDFproperty = RDFproperty Text deriving (Show, Read, Eq, Ord, Generic)
-- ^ a type to identify the RDFproperties
--unRDFproperty (RDFproperty a) = a

--class RDFproperties p where
--    mkRDFproperty :: p -> RDFproperty

newtype RDFtype = RDFtype Text deriving (Show, Read, Eq, Ord, Generic, Zeros)
-- ^ the types for the rdf type values
unRDFtype (RDFtype a) = a

--class RDFtypes p where
--    mkRDFtype :: p -> RDFtype

class Show f => RDFtypes f where
    toIRI :: f -> IRI
    toIRI f = errorT ["toIRI missing in RDFtypes", showT f]
    mkRDFtype ::  f -> RDFtype
    mkRDFtype f = errorT ["mkRDFtype  missing in RDFtypes", showT f]
    mkRDFproperty :: f -> RDFproperty
    mkRDFproperty f = errorT ["mkRDFproperty  missing in RDFtypes", showT f]
    toUNode :: f -> Node
    toUNode f = errorT ["toUNode  missing in RDFtypes", showT f]

instance RDFtypes RDFproperty where
    toIRI (RDFproperty t) = IRI t
    toUNode (RDFproperty p) = unode p
instance RDFtypes RDFtype where
    toIRI (RDFtype t) = IRI t
    toUNode (RDFtype t) = unode t
instance RDFtypes RDFsubj where
    toIRI (RDFsubj s) = s
    toUNode (RDFsubj s) = unode . unIRI $  s
instance RDFtypes IRI where
    toUNode (IRI t) = unode t

    -- TODO change to lowercase initial character

-- class URIs a b where
--   extendURI :: a -> b

newtype RDFsubj = RDFsubj IRI
    deriving (Show, Read, Eq, Ord, Generic, Zeros)
---- ^ a type to identify the RDF subject value
---- is a text, not a URI
--unRDFsubj (RDFsubj a) = a
mkRDFsubj = RDFsubj
--
--subj2unode = unode . unIRI .  unRDFsubj

--instance Zeros RDFsubj where zero = RDFsubj zero

--instance Functor RDFsubj where fmap op (RDFsubj a) = RDFsubj (op a)
-- would applicative work?

--extendHashRDFsubj ::  Text -> RDFsubj ->  RDFsubj
--extendHashRDFsubj e a = RDFsubj .  (<#> e) . unRDFsubj $ a
----extendHashRDFsubj e a = fmap  (<#> e)   a
--extendSlashRDFsubj :: Text -> RDFsubj  -> RDFsubj
--extendSlashRDFsubj e a = RDFsubj .  (</> e) . unRDFsubj $ a
---- makeRDFproperty :: PartURI -> Text -> RDFproperty
---- makeRDFproperty a e = RDFproperty $ a <#> e
---- makeRDFtype :: PartURI -> Text -> RDFtype
---- makeRDFtype a e = RDFtype $ a <#> e



data LanguageCode = NoLanguage | German | USenglish | English
    | French | Spanish | Italian   deriving (Show, Read, Eq, Ord, Generic)
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
            subj = toUNode s
            pred = toUNode p  -- unode (unRDFproperty p)
            obj = lnode (plainL  o )  -- fuer  text symbole

mkTripleLang :: LanguageCode ->  RDFsubj -> RDFproperty -> Text ->  Triple
-- | make a single triple with a language Code
mkTripleLang lg s p o = triple subj pred obj
        where
            subj = toUNode s
            pred = toUNode p
            obj = lnode (plainLL o ( giveCode lg))  -- fuer sprachorientierten text

mkTripleLang3 :: LanguageCode ->  RDFsubj -> RDFproperty -> Text ->  Triple
-- | make a single triple with a language Code using the 3 char codes from wordnet
mkTripleLang3 lg s p o = triple subj pred obj
        where
            subj = toUNode s
            pred = toUNode p
            obj = lnode (plainLL o ( giveCode3 lg))  -- fuer sprachorientierten text

--mkTripleDE1 ::    RDFsubj -> RDFproperty -> Text ->  Triple
---- | make a triple for a german text
--mkTripleDE1 s p o = triple subj pred obj
--        where
--            subj = toUNode s
--            pred = toUNode p
--            obj = lnode (plainLL o "de")  -- fuer deutschen text

mkTripleRef ::    RDFsubj -> RDFproperty -> RDFsubj ->  Triple
mkTripleRef s p o = triple subj pred obj
    where
        subj = toUNode s
        pred = toUNode p
        obj = toUNode o

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
        subj = toUNode s
        pred = toUNode p
        obj = lnode (TypedL (s2t . printf  ['%', '0','1','d'] $ o) integerUri)

zo :: Integer -> Triple
zo n =
    Triple (UNode "http://gerastree.at/kurz#kurz-005")
      (UNode "testint")
      (LNode (TypedL (showT n) integerUri))

-- to demonstrate error i rdf4h
--test_typed = assertEqual (TypedL "0" integerUri)
--            (typedL "0" integerUri)


rdfsURI =  "http://www.w3.org/2000/01/rdf-schema#"
rdfURI =  "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

mkTripleType :: RDFsubj -> RDFtype -> Triple
-- ^ make a triple with the rdf type = a
mkTripleType s  o = triple subj pred obj
    where
        subj = toUNode s
        pred = unode $ rdfURI <> "type"
        obj = unode . unRDFtype $ o

mkTriplePartOf :: RDFsubj -> RDFsubj -> Triple
-- ^ make a triple with the rdfs partOf predicate
mkTriplePartOf s  o = triple subj pred obj
    where
        subj = toUNode s
        pred = unode $ rdfsURI <> "partOf"
        obj = toUNode o
