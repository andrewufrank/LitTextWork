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


module Data.RDF.Extension_test  where


import           Test.Framework
import Text.Printf
import Data.RDF.Extension

--import           Data.Map            as Map (fromList)
--import           Data.RDF            (Node, Triple (..), lnode, objectOf,
--                                      plainL, plainLL, triple, typedL, unode)
--import           Data.RDF            as RDF
--import qualified Data.RDF            as RDF
--import           Data.RDF.Prefs
--import qualified Data.RDF.Types      as RDF (RDF (..), RdfSerializer (..))
---- import           Data.Text           hiding (map)
---- import qualified Data.Text           as T (append, concat, null, strip)
--import           Uniform.Error
---- import           Uniform.FileIO      (thd3)
---- import           Uniform.StringInfix ((</>))
--import           Uniform.Strings
--import           Uniform.Zero
----import           Uniform.Convenience.LitTypes


test_typed0 = assertEqual (zo 0)
            (mkTripleInteger s1 r1 0 )
test_typed100 = assertEqual (zo 100)
            (mkTripleInteger s1 r1 100 )
test_typedneg1 = assertEqual (zo (-1))
            (mkTripleInteger s1 r1 (-1) )
test_typedneg20 = assertEqual (zo (-20))
            (mkTripleInteger s1 r1 (-20) )


