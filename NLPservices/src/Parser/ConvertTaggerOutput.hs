-----------------------------------------------------------------------------
--
-- Module      :  Convert the output from the tagger to simple structure
-- Copyright   : af
--
--

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Parser.ConvertTaggerOutput (convertTT
    , TTdata (..)
    , htf_thisModulesTests
    )   where

import           Test.Framework
--import           Test.Invariant
import Uniform.Strings
import Uniform.Error
import Uniform.Zero
import qualified System.IO as IO
--import NLP.CallTagger2


data TTdata = TTdata {ttwf::Text  -- the input wordform
                      , ttpos :: Text  -- the tree tager POS value
                      , ttlemma :: Text }
              | TTcode Text   -- the code ending the sentence
              | TTpage Text Text -- a page number code
                      deriving (Show, Eq)
                      -- the code adn page is actually not used
                      -- produced only when SGML tags in the input

-- putIOwordsT :: [Text] -> ErrIO ()
-- putIOwordsT = putIOwords

convertTT :: Text -> [TTdata]
convertTT = map (toTT . words') . lines'

toTT :: [Text] -> TTdata
-- ^ parse one line of the result
toTT [w,p,l] = TTdata {ttwf=w, ttpos=p, ttlemma=l}
toTT [c,n] = TTpage c n
toTT [c] = TTcode c
toTT x = errorT ["toTT not expecting", unwords' x]


