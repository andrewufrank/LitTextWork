-----------------------------------------------------------------------------
--
-- Module      :  LitText.Foundation . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | the definitions of the descrption of the text and related types (e.g. Language)
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances
    , DeriveGeneric
    , RecordWildCards
    , DeriveAnyClass #-}

module LitText.Parser.Parser  (
        produceLayoutTriples
        ,textstate2Text
                        , LitTextFlag (..), LitTextFlags (..)
                        , LitTextFlagSet
                        , TextDescriptor (..)
                        , produceLitTriples, Triple
                        , produceNLPtriples, tz2toSnip, Snip
                        , prepareTZ4nlp, formSnips
        -- for tests:

                        )  where

import LitText.Parser.FilterTextForNLP
import LitText.Parser.FormNLPsnips
import LitText.Parser.ProduceLayout
import LitText.Parser.ProduceLit
import LitText.Parser.ProduceNLP
import LitText.Parser.ReadMarkupAB

