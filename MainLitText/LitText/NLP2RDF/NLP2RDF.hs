-----------------------------------------------------------------------------
--
-- Module      :  LitText.Foundation . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | export for the tests
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances
    , DeriveGeneric
    , RecordWildCards
    , DeriveAnyClass #-}

module LitText.NLP2RDF.NLP2RDF  (
    convertOneSnip2triples_NLPservices
    , LTtext (..), LanguageTyped2(..), LanguageTypedText, typeText
    , undefEnglish, undefGerman, undefFrench, undefItalian, undefSpanish
    , snip2triples, triple2text, text2nlpCode, unLCtext, nlpPath, addPort2ServerURI
    , serverBrest
    , RDFsubj-- , ParaSigl
    )  where

import LitText.NLP2RDF.LanguageSpecific
import LitText.NLP2RDF.ProduceDocCallNLP
--

