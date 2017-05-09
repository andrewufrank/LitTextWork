-----------------------------------------------------------------------------

--
-- Module      :  Store.RDFstore.PrefixMap
--
-- | the stuff to map the prefix
-- collect here all the code for prefixes
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- -fno-warn-missing-methods

module Store.RDFstore.PrefixMap   where

import           Uniform.Strings

import qualified Data.List       as L (find)
import           Data.Map        (Map)
import qualified Data.Map        as Map (toList)
import           Data.RDF
import qualified Data.RDF.Types  as RDF (PrefixMappings (..))
import qualified Data.Text       as T (Text, drop, isPrefixOf, length)
--import Data.RDF.Extension ((<:>))


uri2prefixConv ::  RDF.PrefixMappings  ->  T.Text -> T.Text
uri2prefixConv (RDF.PrefixMappings prefixes) t =
--  putStrLn . unwords $  ["writeUNodeUri mapping is ", show mapping ]
  case mapping of
    Nothing                 -> t
    (Just (pre, localName)) -> pre <:> localName
    -- the prefix includes the :
--    (Just (pre, localName)) -> T.hPutStr h pre >> hPutChar h ':' >> T.hPutStr h localName
  where
    mapping         = findMappingRev prefixes t

-- reversed from:
-- Expects a map from uri to prefix, and returns the (prefix, uri_expansion)
-- from the mappings such that uri_expansion is a prefix of uri, or Nothing if
-- there is no such mapping. This function does a linear-time search over the
-- map, but the prefix mappings should always be very small, so it's okay for now.
findMappingRev :: Map T.Text T.Text -> T.Text -> Maybe (T.Text, T.Text)
findMappingRev pms uri =
  case mapping of
    Nothing     -> Nothing
    Just (p, u) -> -- error (show p ++ "--" ++  show u) --
                    Just (p, T.drop (T.length u) uri) -- empty localName is permitted
  where
    mapping :: Maybe (T.Text, T.Text)
    mapping        =   L.find (\(_,u) -> T.isPrefixOf u uri) (Map.toList pms)
-- exchanged _ and k: the map is from uri to prefix, check for  k match as prefix to uri
-- it was reversed in writeTriples
