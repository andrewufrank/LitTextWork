-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | produce the triples for the NLP part
-- the analysis is completed and stored in doc0
-- this is only producing the triples, converted from doc0

-- the snip has an id (which is the paraid of the first paragraph
-- snips are not separated in paragraphs but the sentences are part of the snip and the snip part of the book



-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module LitText.CoreNLP.DocBase_test  where

import           Test.Framework
progName = "nlpservices"


