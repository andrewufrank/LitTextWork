-----------------------------------------------------------------------------
--
-- Module      :  RDFeditor.Prefs
-- | prefs here are
--      the prefixes
--      the endpoints
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.RDF.Prefs
(module Data.RDF.Prefs
-- , module Uniform.HttpGet
, module Uniform.Strings
, module Uniform.Error)  where

-- import           Safe
import           Uniform.Error   -- (fromJustNote)
import           Uniform.Strings
import           Uniform.Zero
-- import Uniform.HttpGet  -- for URI
-- the URI here is just text - URItext

import qualified Data.Map        as Map (fromList)
import           Data.Maybe      (listToMaybe)
import qualified Data.RDF        as RDF (PrefixMappings (..))


-- | an entity of two parts,
-- the internal structure of the entity is  shown
-- list
class Entity2 f a b where
    mk2 :: a -> b -> f a b
--    check2 :: a -> b -> f a b -> Bool
    add2 :: a -> b -> [f a b] -> [f a b]
    add2 a b f = (mk2 a b) : f
    remove2 :: a -> [f a b] -> [f a b]
    find2 :: a -> [f a b] -> Maybe b



instance Entity2 PrefixPairX Text URItext where
    mk2 a b = PrefixPair a b
    remove2 a = filter ( (a/=) . pfCode)
    find2 a =   fmap pfURI  -- . fromJustGuarded "endpoints not found"
                . listToMaybe . filter ( (a==) . pfCode)

mkPrefixPair :: Text -> URItext -> PrefixPair
mkPrefixPair = mk2

instance Entity2 EndptsPairX Text URItext where
    mk2 = EndptsPairX
    remove2 a = filter ( (a/=) . epCode)
    find2 a =   fmap epURI .   listToMaybe . filter ( (a==) . epCode)

reorderEndpts a eps = add2 a b . remove2 a $ eps
    where  b = fromJustNote "reorderEndpts" . find2 a $  eps

--type EndPoint = String  -- from connection
type URItext = Text  -- just the uri, not the <..>

data RDFeditorPrefs = RDFeditorPrefs { prefix :: [PrefixPair]
                                , endpts      :: [EndptsPair]
                                } deriving (Show,  Eq, Ord)

type PrefixPair = PrefixPairX Text URItext
--{prefixShort::Text, prefixVal ::Text}
data PrefixPairX a b = PrefixPair
                        { pfCode :: a  -- Text  -- does not include the ":"
                        , pfURI  ::  b --  URItext
                                    } deriving (Show,  Eq, Ord)
                                    -- TODO is twice the same structure as EndptsPair !

instance Zeros PrefixPair where zero = mk2 zero zero

type EndptsPair = EndptsPairX Text URItext
data EndptsPairX a b  = EndptsPairX {epCode :: a
                            , epURI         ::  b
                            } deriving (Show, Read, Eq, Ord)

instance Zeros EndptsPair where zero = mk2 zero zero


defaultPrefs = RDFeditorPrefs [] []

startPrefs = RDFeditorPrefs startPrefix startEndpts

startPrefix = [
            --   mk2 "text" "http://4store.org/fulltext#"
              mk2 "text" "http://gerastree.at/fulltext#"
            -- , mk2 "lit" "http://auf.us.to/lit_2014#"
            , aichingerPrefix
            -- , mk2 "aichinger" "http://auf.us.to/aichinger_2014#"
            , mk2 "aichinger" "http://gerastree.at/aichinger_2014#"
            , mk2 "dbres" "http://dbpedia.org/resource/"
            , mk2 "xsd" "http://wwww.w3.org/2001/XMLSchema#"
            , mk2 "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
            , mk2 "rdfs1999" "http://www.w3.org/1999/02/22-rdf-schema#"
            ] :: [PrefixPair]


aichingerPrefix = PrefixPair ""  "http://gerastree.at/aichinger_2014#"
-- ist meist der default prefix

dove = "dove" ::Text
dovePrefix = mkPrefixPair dove  "http://gerastree.at/dove_2017#"

dovePrefixes = add2  "dbres" "http://dbpedia.org/resource/"
                $ add2 "xsd" "http://wwww.w3.org/2001/XMLSchema#"
                $ add2 "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                $ add2 "rdfs1999" "http://www.w3.org/1999/02/22-rdf-schema#"
                [dovePrefix] :: [PrefixPair]

--t2oPrefix = "http://gerastree.at/t2o/"
--nlpPrefix = "http://gerastree.at/nlp2015/"


-- what is the use of these - change for virtuoso
startEndpts = [  ]
            -- mk2 "demo4" "http://127.0.0.1:8004"
--                 , mk2 "demo" "http://127.0.0.1:8000"
--                 , mk2 "aichinger" "http://127.0.0.1:8010"
--                 ]

prefix2query :: PrefixPair   -> Text
-- convert the prefix pair to the form in a query
prefix2query pp =  concat' [ "prefix ", pfCode  pp , ": <" , pfURI pp , ">" ]
-- some method to start 4store on these?
-- example PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

prefixs2qtext :: [PrefixPair] -> Text
prefixs2qtext ps = unlines' . map prefix2query $ ps

prefix2ttl :: PrefixPair   -> Text
-- convert the prefix pair to the form in a turtle
prefix2ttl pp =   concat' [ "@prefix " , pfCode  pp , ": <" , pfURI pp , "> ." ]
-- example @prefix lit:   <http://auf.us.to/lit_2014#> .

prefixs2ttl :: [PrefixPair] -> Text
prefixs2ttl ps = unlines' . map prefix2ttl $ ps

prefix2pair :: PrefixPair -> (Text, Text)
prefix2pair p  = ( pfCode $ p,   pfURI $ p)

prefixs2pm :: [PrefixPair] -> RDF.PrefixMappings
prefixs2pm ps = RDF.PrefixMappings $  Map.fromList . map prefix2pair $ ps
