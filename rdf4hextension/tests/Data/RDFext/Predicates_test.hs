-- | extension for predicates on triples
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns  #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.RDFext.Predicates
    (module Data.RDFext.Predicates
    -- isP
    -- , getURI4Node, getText4node
--    , nlp, t2o -- export only the prefixes, because ntriple are construcyted
--    , nlpPrefix
--    , t2oPrefix, litPrefix   -- move to littext
--    , nlpURI
--    , t2oURI, litURI
--    , convertPrefixPair2PrefixMapping
    )
    where


-- import           Data.Text       hiding (map)
-- import qualified Data.Text       as T (append, concat, null, strip)

-- import           Safe
import           Uniform.Error
-- import           Uniform.Strings
import           Uniform.Zero

import           Data.Map        as Map (fromList)
import           Data.RDF
        -- (Triple, Node)
                    -- lnode, plainL, plainLL, triple,
                    --               typedL, unode)
import qualified Data.RDF        as RDF
import qualified Data.RDF.Types  as RDF (RDF (..), RdfSerializer (..))
-- import qualified System.IO as S (hClose, IOMode(..), openFile)
import           Data.RDF.Prefs


-- a set of tests for s, p and o of a triple
-- to be used in filter a list of triples
-- check if rdf4h select could be used?

isP :: (Node -> Bool)  -> Triple -> Bool
isP cond = cond . predicateOf

getText4node :: Node -> Text
-- get the text from a PlainL or PlainLL node -- language is not checked
-- this is duplicated somewhere TODO
getText4node (LNode (PlainL t) )   = t
getText4node (LNode (PlainLL t l)) = t
getText4node x                     = errorT ["not a PlainL or LL", showT x]

getURI4Node :: Node -> Text -- PartURI
-- ^ get the uri from a unode
getURI4Node (UNode x) = x
getURI4Node z         = errorT ["getURI4Node not UNode", showT z]
