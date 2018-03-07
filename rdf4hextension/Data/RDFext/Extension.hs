-- | extension for triple making and similar
-- but not the codes for the relations - these go to the specific
-- construction programs, to link directly with the definitions
-- and not the prefix stuff, which goes to prefs
--
-- the URI are always open at end and connecting must add separator
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns  #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.RDFext.Extension (
    module Data.RDFext.Extension
    , module Data.RDF
    , module Data.RDFext.Prefs
    , module Data.RDFext.FileTypes
    , module Data.RDFext.Predicates
    , module  Data.RDFext.Triple2text
    , module Data.RDFext.Codes
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

