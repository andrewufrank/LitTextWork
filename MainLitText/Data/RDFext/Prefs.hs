-----------------------------------------------------------------------------
--
-- Module      :  RDFeditor.Prefs
-- | prefs here are
--      the prefixes
--      the endpoints
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
--{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric
    , DeriveAnyClass  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.RDFext.Prefs
    (module Data.RDFext.Prefs
    , module Uniform.Error)  where

import           Uniform.Error   -- (fromJustNote)
import           Uniform.Zero

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
                                } deriving (Show,  Eq, Ord, Generic, Zeros)

type PrefixPair = PrefixPairX Text URItext
--{prefixShort::Text, prefixVal ::Text}
data PrefixPairX a b = PrefixPair
                        { pfCode :: a  -- Text  -- does not include the ":"
                        , pfURI  ::  b --  URItext
                                    }
                    deriving (Show,  Eq, Ord, Generic, Zeros)
                                    -- TODO is twice the same structure as EndptsPair !

--instance Zeros PrefixPair where zero = mk2 zero zero

type EndptsPair = EndptsPairX Text URItext
data EndptsPairX a b  = EndptsPairX {epCode :: a
                            , epURI         ::  b
                            }
                deriving (Show, Read, Eq, Ord, Generic,Zeros)

--instance Zeros EndptsPair where zero = mk2 zero zero



