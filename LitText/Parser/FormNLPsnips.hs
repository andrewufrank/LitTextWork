 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FormNLPsnips  D -> DA
-- Copyright   :  andrew u frank -
--
-- | form pieces of literal text which are reasonably sized

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.FormNLPsnips
     (module Parser.FormNLPsnips
     , module LitTypes.TextDescriptor
     )
    where

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
mergeNLPtext a b = if sameLang a b && (snip3textLength a + snip3textLength b) < maxSnipSize
                        then Just (a {snip3text = fromJustNote "mergeNLPtext "
                                         (mergeLC " " (snip3text a) (snip3text b))
--                                snip3text = snip3text a <> " " <> snip3text b
                                , snip3textLength = snip3textLength a + snip3textLength b +1})
                                    -- there is a spaece added, would a period "." be needed?
                        else Nothing

    where
        sameLang a b = sameLanguageLC (snip3text a) (snip3text b)



