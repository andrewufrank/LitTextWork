 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FormNLPsnips  D -> DA
-- Copyright   :  andrew u frank -
--
-- | form pieces of literal text which are reasonably sized

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.FormNLPsnips
    where

--import Lines2para.Lines2para
--import Lines2para.HandleLayout
--import Parser.ReadMarkupAB  -- todo  -- for test
--import LitTypes.ServerNames
--import           CoreNLP.Defs0
--import CoreNLP.CoreNLPxml (readDocString)
--import Data.List.Split
--import Uniform.HttpCall (makeHttpPost7, addPort2URI)
--import Text.Regex (mkRegex, subRegex)
--import Parser.FilterTextForNLP
import Uniform.Error (fromJustNote)
import LitTypes.TextDescriptor

formSnips :: [Snip] -> [Snip]
-- collect paragraphis in reasonalbe snips for NLP processing
formSnips [] = []
formSnips [n] = [n]
formSnips (n1:n2:ns) =  case mergeNLPtext n1 n2 of
        Nothing -> n1 : formSnips (n2:ns)
        Just ab ->  formSnips (ab:ns)


minSnipSize = 5000 -- 1000 -- 5000 -- char
maxSnipSize = 10000 -- 2000  -- 10000


mergeNLPtext :: Snip -> Snip -> Maybe Snip
-- merge two text if same language and size less than maxSnipSize
mergeNLPtext a b = if sameLang a b && (tz3textLength a + tz3textLength b) < maxSnipSize
                        then Just (a {tz3text = fromJustNote "mergeNLPtext "
                                         (mergeLC " " (tz3text a) (tz3text b))
--                                tz3text = tz3text a <> " " <> tz3text b
                                , tz3textLength = tz3textLength a + tz3textLength b +1})
                                    -- there is a spaece added, would a period "." be needed?
                        else Nothing

    where
--        alength = lengthChar . tz3text $ a
--        blength = lengthChar . tz3text $ b
        sameLang a b = sameLanguageLC (tz3text a) (tz3text b)



