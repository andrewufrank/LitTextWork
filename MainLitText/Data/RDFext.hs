-- | the top level export

{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns  #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.RDFext (
      module Data.RDFext.Prefs
    , module Data.RDFext.FileTypes
    , module Data.RDFext.Predicates
    , module  Data.RDFext.Triple2text
    -- , module Data.RDFext.Codes
    , IRI, mkIRI, append2IRI, append2IRIwithSlash, unIRI
--    , mkHttPathFromIRI
    , LanguageCode (..)  --
--    , RDFproperties (..)
    , RDFtypes (..)
    , RDFsubj, mkRDFsubj
    , RDFproperty, RDFtype
    , GraphName, mkGraphName, unGraphName
            -- to convert to query param
    , RDFdataset, mkRDFdataset
    , PartURI  -- should be removed
    , mkTripleText, mkTripleLang, mkTripleLang3
    , mkTripleRef, mkTripleInt, mkTripleInteger
    , mkTripleType, mkTriplePartOf
    , parseLanguageCode, readLanguageCode
    , module Uniform.FileIO
    )     where

import           Data.RDF            (Node, Triple (..), lnode, objectOf,
                                      plainL, plainLL, triple, typedL, unode)
import           Data.RDF            as RDF
import qualified Data.RDF            as RDF

-- from rdf4hextensionsimport           Data.RDF.Prefs
import Data.RDFext.Prefs
import Data.RDFext.FileTypes  hiding ((<>) , (</>), (<.>))
import Data.RDFext.Predicates
import Data.RDFext.Triple2text
import Data.RDFext.Codes
import Uniform.FileIO hiding ((<>) , (</>), (<.>))

