{-----------------------------------------------------------------------------
--
-- Module      :  convert the text snippets (hl1 pieces) to nt with coreNLP
-- and store the RDF triples with SPARQL insert
--
-- | not used
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}
--{-# LANGUAGE DeriveAnyClass        #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE FlexibleInstances     #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE OverloadedStrings     #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeFamilies          #-}
--{-# LANGUAGE TypeSynonymInstances  #-}

module xCoreNLP.ReadDoc0 (
       module CoreNLP.ReadDoc0
        -- , htf_thisModulesTests
--        , makeNLPrequest5
--        ,makeNLPrequest6
        -- , readDocString
        -- , nlp_serverLoc, host_serverLoc
        )  where

import           Test.Framework

import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings
import           Uniform.Zero
import Uniform.HttpGet

import           CoreNLP.CoreNLPxml

-- import           Data.RDF                (Triple, mkRdf, triplesOf)
-- import           Data.RDF.Extension      (gerastreeURI, PartURI)
-- import           Data.RDF.FileTypes      (RDFgraph (..), ntFile, unRDFgraph)
-- import           Data.RDF.Prefs          (prefixs2pm, startPrefix)

-- import           Data.Text.Encoding
import           Text.XML.HXT.Core       hiding (when)


readDocString :: Bool -> Text  -> ErrIO  Doc0
readDocString showXML text = do
  docs  :: [Doc0] <-callIO $ do
        d1 :: [Doc0] <-  runX (readString [withValidate no]  (t2s text)
                                >>> getDoc0)
        return d1
  when showXML $ do
      putIOwords ["the xml formated"]
      res <- callIO $ runX . xshow $ readString [withValidate no]  (t2s text)
                                        >>> indentDoc
      putIOwords  $ map s2t res
      putIOwords ["the xml formated ---------------------"]

--  let toks2 = filter ((0 /=). sid) toks
  -- seems to add a lot of empty sentences

  if (length docs) > 1
        then error "multiple document tags"
        else  do
--            putIOwords ["readDocString - the xml read"]
            return (headNote "no document found" docs)
            -- error in case of 0
