-----------------------------------------------------------------------------
--
-- Module      :  Main.BuchToken

--
-- | the different types of text in a markup file
-- TODO would it be ossible to select lines with all caps as hl1?
-- recognize the ending of gutenberg to ignore?
-- uebersetzung und uebersetzer?
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module BuchCode.BuchToken (module BuchCode.BuchToken
        , module Uniform.Error
        , module Data.RDF.Extension 
            --     BuchTokenized(..)
            -- , BuchToken (..), unusedTokens, tokenNLPanalysed
            -- , Unparser (..)
            -- , MarkupElement, mkBuchTKwithoutnum, numberBuchTK
            -- , LanguageCode (..)
            ) where

import           Data.RDF.Extension -- (LanguageCode (..), PartURI)
import           Uniform.Error
--import           Uniform.Strings
--import Safe  -- is export from Error
import Parser.Foundation


data BuchTokenized a = BuchTokenized { btokenType:: a
                                    , btokenText  :: Text
                                    , btokenID    :: Int
                                    , btokenLang  :: LanguageCode
                                    }  deriving (Eq, Show)

mkBuchTKwithoutnum :: BuchToken -> Text -> MarkupElement
mkBuchTKwithoutnum mk t = BuchTokenized {btokenType = mk, btokenText = t
        , btokenID = undef "mkBuchwithoutnum id", btokenLang = English}
-- fill in the text, number later

numberBuchTK bk i = bk {btokenID = i}
-- add the numbers when all found

--type Markup = [BuchTokenized BuchToken]

type MarkupElement = BuchTokenized BuchToken

data BuchToken =   -- just the markers
-- attention: shorter strings must be before longer ones (with same start)
-- the markup marker are derived from the names (strip Buch, convert to lower)
          BuchIgnoreEnd   -- only for mark, no output

        | BuchIgnoreTo
        | BuchIgnore
    -- the following codes are automatically parsed with .XX for BuchXX
        | BuchKlappenText
        | BuchAuthorLeben
        | BuchDanksagung
        | BuchAuthor
        | BuchUntertitel
        | BuchVerlag
        | BuchPublikationDetail
        | BuchEntstehungsDatum
        | BuchDedikation
        | BuchFussnote
        | BuchCopyright
        | BuchISBN
        | BuchQuelle  -- woher ist der text (z.b. gutenberg)
        | BuchFilename        -- bei der Quelle URL oder aehnlich, filename..
        | BuchSprache        -- to mark following text with the language
        | Buchuebesetzer
        | BuchIV1
        | BuchIV2
        | BuchIV3
        | BuchTitel
        | BuchHL1
        | BuchHL2
        | BuchHL3
        | BuchEnde
    -- special parses
        | BuchParagraph
        | BuchParagraphLayout  -- not input, property only
        | BuchGedicht
        | BuchLeer
        | BuchPropBuch    -- fuer turtle code (prefix Buchsigl)
        | BuchPropText    -- fuer turtle code (prefix TextSigl)
                 deriving (Enum, Eq, Show)


tokenNLPanalysed = [BuchHL1, BuchHL2, BuchHL3, BuchGedicht, BuchParagraph]
unusedTokens = [BuchLeer, BuchEnde, BuchIgnore, BuchIgnoreTo
                , BuchCopyright ]

-- | unparse a sequence of tokens in a sequence of texts
class Unparser  a where
--    unparse :: BuchTokenized a  -> Text
--    -- ^ to output a parsed text close to the original (op = op.op)
    marker :: a -> Text
    markerPure :: a -> Text
    -- ^ to convert a buchtoken to the markup text (lowercase)
    -- for special cases is an upper case version produced
    -- (or use metaparsec)
    markerPureMult :: a -> [Text]
    -- ^ give alternatives for the marker
    -- attention: no names where the start could be the value of a previously checked token


instance Unparser BuchToken where
    -- longer forms must come first for correct parse
    markerPure  = toLower' . fromJustNote "not Buch in marker name" . stripPrefix' "Buch" . showT
    marker  =   ("." <>) . markerPure
    markerPureMult BuchSprache = [ "language:", markerPure BuchSprache, "language"]
    markerPureMult BuchTitel   = [ "title:", markerPure BuchTitel, "title"]
    markerPureMult BuchUntertitel   = [ "subtitle:", markerPure BuchUntertitel, "subtitle"]
    markerPureMult BuchFussnote   = [ "footnote", markerPure BuchFussnote]
    markerPureMult BuchDedikation   = [ "dedication", markerPure BuchDedikation]

    markerPureMult BuchPublikationDetail = [markerPure BuchPublikationDetail, "publicationDetail"
                                , "publikationsdetail"]
    markerPureMult a           = [markerPure a]
