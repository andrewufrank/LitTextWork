-----------------------------------------------------------------------------
--
-- Module      :  a conversion of an ntriple to a text
-- adapted from rdf4h Text.RDF.RDF4H.NTriplesSerializer
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables
    , StandaloneDeriving        #-}
--{-# LANGUAGE TypeSynonymInstances       #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.RDFext.Triple2text
    (module Data.RDFext.Triple2text
    , RDF.Triple
    -- , module Data.RDF   -- is imported qualified
    ) where

import Uniform.Strings

import           Data.RDF        as RDF (LValue (..), Node (..), Triple (..),
                                         triple, LValue (..))

triple2text :: Triple -> Text
triple2text (Triple n1 n2 n3) = unwords' [(node2text n1),  (node2text n2),  (node2text n3), " ."]

node2text :: Node -> Text
node2text (UNode t) = wrapInSpitz t
node2text (BNode t) =  t
node2text (BNodeGen i) = ":genid" <> showT i
node2text (LNode l) = lnode2text l

lnode2text :: LValue -> Text
lnode2text (PlainL t) = wrapInDoubleQuotes . writeLiteralString $ t
lnode2text (PlainLL t l) =  (wrapInDoubleQuotes . writeLiteralString $ t) <> "@" <>  l
lnode2text (TypedL t l) = (wrapInDoubleQuotes . writeLiteralString $ t) <> "^^" <> wrapInSpitz l
--node2text (PlainL t) = t

-- TODO: this is REALLY slow.
writeLiteralString::  Text -> Text
writeLiteralString  i  =   concat' . map  convertChar . t2s $ i

convertChar :: Char -> Text
-- convert cr lf and " in string -- https://www.w3.org/TR/n-triples/#sec-literals
--  https://www.w3.org/2001/sw/RDFCore/ntriples/
convertChar c =
      case c of
        '\n' ->   "\\n"
        '\t' ->  "\\t"
        '\r' -> "\\r"
        '"'  ->  "\\\""
        '\\' ->  "\\\\"
        _  ->  s2t [c]



