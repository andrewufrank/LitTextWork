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


--import           Test.Framework
--import Text.Printf
--import           Data.Map            as Map (fromList)
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

--import qualified Data.RDF.Types      as RDF (RDF (..), RdfSerializer (..))

-- import           Data.Text           hiding (map)
-- import qualified Data.Text           as T (append, concat, null, strip)
--import           Uniform.Error
-- import           Uniform.FileIO      (thd3)
-- import           Uniform.StringInfix ((</>))
--import           Uniform.Strings ((</>))
--import           Uniform.Zero
----import           Uniform.Convenience.LitTypes

