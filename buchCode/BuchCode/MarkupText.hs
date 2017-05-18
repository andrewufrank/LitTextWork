-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines
-- Copyright   :  andrew u frank -
--
-- | a parser for a line oriented file
-- especially with lines starting with a markup char ('.')
-- following the example in Real World Haskell

-- this produces a list of encoded lines TextZeilen but not yet blocks.
-- ignore is parsed as a markup
-- to use automatic hl2 detection - replace in gutenberg ".--" -- not required anymore

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module BuchCode.MarkupText ( TextZeilen (..)
    , TextType (..)
    , Zeilen (..)
    , BuchToken (..)
    , parseMarkup
    , countSeiten
    , countLeerzeilen
    , readSeitenzahl, checkSeitenzahlen
    , averageLengthTextLines
    , combine2linesWithHyphenation
        , htf_thisModulesTests   -- for tests
        , result0B, result0BA
        , result1B, result1BA
        , result2B, result2BA
        , result3B, result3BA
        , result4B, result4BA

    ) where


import           BuchCode.BuchToken
import           Data.Char
import           Text.Parsec
import           Uniform.Error
--import           Uniform.FileIO hiding ((<|>))
import           Uniform.Strings   hiding ((<|>))
import           Test.Framework


class Zeilen z where
-- ^ ops on zeilen
    isLeerzeile
        , isSeitenzahl
        , isTextZeile   -- ^ any line containing lit text
        , isNeueSeite
        , isMarkupZeile
        , isKurzeZeile  :: z -> Bool
    isMarkupX :: BuchToken -> z -> Bool
    zeilenText :: z -> Text
    -- returns just the text field

    renderZeile :: z -> Text

type TextParsec a = Parsec Text () a
type TextParsecBuch = Parsec Text () [MarkupElement]

-- the type of the line
data TextType = Text0 | Zahl0 | Fussnote0 | Kurz0 | Para0 | AllCaps0
        deriving (Show, Eq )


-- the kinds of lines differentiated
-- changes here must be reflected in the programs
-- using the data, e.g Lines2para in etts2tz
data TextZeilen =  TextZeile {ttt::TextType, ttx::Text}
--        | ZahlZeile Text
--        | FussnoteZeile Text
        | MarkupZeile {ttok:: BuchToken, ttx:: Text}
--        | KurzeZeile Text
--        | ParaZeile  Text
        -- ^ a line which is a paragraph, not mergable with other
        -- could be used for poems as well
        -- but usually poems are automatically recognized by short lines
--        | AllCapsZeile Text   -- ^ to recognize titles automatically
        | LeerZeile
        | NeueSeite
        deriving (Show, Eq )


parseMarkup :: Text ->  [TextZeilen]  -- test B -> BA
-- parse a text file to TextZeilen form
parseMarkup  =  markShortLines  . markAllCapsLines
            . parseMarkupText . s2t . filter (/= '\r') . t2s
-- TODO filter for char

test_0B_BA = assertEqual result0BA (parseMarkup result0B)
test_1B_BA = assertEqual result1BA (parseMarkup result1B)
test_2B_BA = assertEqual result2BA (parseMarkup result2B)
test_3B_BA = assertEqual result3BA (parseMarkup result3B)
test_4B_BA = assertEqual result4BA (parseMarkup result4B)
test_5B_BA = assertEqual result5BA (parseMarkup result5B)


renderETTs :: [TextZeilen] -> Text
-- renders the lines
renderETTs  = unlines' . map renderZeile

parseMarkupText :: Text ->   [TextZeilen]
-- mark the page numbers
parseMarkupText tx =
    case parse fileParser "" tx  of
       Left err -> errorT ["\nParseError" ,
                 showT . errorPos $ err , showT err
                   ]
       Right ans ->     ans


fileParser :: TextParsec [TextZeilen]
fileParser = do
    optional (char '\65279')  -- to remove BOM if present
    res <- many lineParser
    eof
    return res

lineParser :: TextParsec TextZeilen
lineParser = do
--    res <- ettParser
----    newline   -- zeile is limited by newline
--    return res
--
--ettParser :: TextParsec TextZeilen
--ettParser = do
    try leerzeile
    <|>
    try neueSeite
    <|>
    try zahlzeile
    <|>
    try fussnotezeile
    <|>
    try markupzeile
    <|>
--    try allCapsZeile   -- identified later - better?
--    <|>
    try textzeile
        <?> "lineparser"



zahlzeile :: TextParsec TextZeilen
zahlzeile = do
    res <- many1  (oneOf "0123456789[]/")
                -- these are characters encountered in german textarchive
--    res <- many1  digit  - simpler, but not including []
    many1 newline
    return . TextZeile Zahl0 . s2t $ res

fussnotezeile :: TextParsec TextZeilen
fussnotezeile = do
    res <- many1  (oneOf "0123456789[")
    res1 <- oneOf ")]"
                -- these are characters encountered in german textarchive
--    res <- many1  digit  - simpler, but not including []
    res2 <- many (noneOf "\n")
    many1 newline
    return . TextZeile Fussnote0 . s2t $ (res ++ [res1] ++ res2)

markupzeile :: TextParsec TextZeilen
markupzeile = do
    char '.'
    mk <- choice ( map (\t -> try (tokenElement t)) [BuchIgnoreEnd .. BuchEnde] )
    res <- many (noneOf "\n")
    newline
    return . MarkupZeile mk . s2t . trim' $ res

--allCapsZeile  :: TextParsec TextZeilen
--allCapsZeile = do
--    res <- many (satisfy (\c -> isUpper c && ('\n' /= c)))  -- wichtig: do not consume end of line
--    newline
--    return . AllCapsZeile . s2t $ res

textzeile :: TextParsec TextZeilen
textzeile = do
    res <- many (noneOf "\n")  -- wichtig: do not consume end of line
    newline
    return . TextZeile Text0 . s2t $ res

leerzeile :: TextParsec TextZeilen
leerzeile = do
    newline
    return LeerZeile   <?> "Leerzeile"

neueSeite :: TextParsec TextZeilen
neueSeite = do
    char '\f'
    newline
    return NeueSeite <?> "NeueSeite"

 -- tokenELemen (i.e. Titel...) = ".Token" space {space} Multiline
tokenElement :: BuchToken -> TextParsec BuchToken
tokenElement mk =   do
            alt <- choice (map (\t -> try (tokenElementOne t)) (markerPureMult mk))
            return mk
-- tokenELemen (i.e. Titel...) = ".Token" space {space} Multiline
tokenElementOne :: Text -> TextParsec Text
tokenElementOne mk =   do
            caseInsensitiveString mk
            return mk

-- GenParser Char state Char
caseInsensitiveChar :: Char -> TextParsec Char
-- copied from parsec extra
caseInsensitiveChar c = do
      char (toLower c) <|> char (toUpper c)
      return c

-- | Parse the given string, but with any combination of upper and lower case
-- characters.
-- caseInsensitiveString :: String -> GenParser Char state String
caseInsensitiveString :: Text -> TextParsec Text
caseInsensitiveString = fmap s2t . sequence . map caseInsensitiveChar . t2s

combine2linesWithHyphenation :: Text -> Text -> Text
-- combine two words or parts of words
combine2linesWithHyphenation a b = maybe (unwordsT [a,b])  (<>b)  $ stripSuffix' "-" a


countSeiten :: [TextZeilen] -> Int
countSeiten = length . filter isSeitenzahl

countLeerzeilen :: [TextZeilen] -> Int
countLeerzeilen = length . filter isLeerzeile


instance Zeilen TextZeilen where
    isLeerzeile LeerZeile = True
    isLeerzeile _         = False

    isSeitenzahl (TextZeile Zahl0 _) = True
    isSeitenzahl _ = False

    isTextZeile (TextZeile Text0 _) = True   -- can include footnote text
    isTextZeile (TextZeile Kurz0 _) = True
    isTextZeile (TextZeile Para0 _) = True
    isTextZeile (TextZeile AllCaps0 _) = True
    isTextZeile _             = False

    isNeueSeite (NeueSeite) = True
    isNeueSeite _ = False

    isMarkupZeile (MarkupZeile mk t) = True
    isMarkupZeile _                  = False

    isMarkupX code (MarkupZeile mk t) =  code == mk
    isMarkupX _ _                     = False

    isKurzeZeile = (Kurz0 ==).ttt

    -- zeilenText (TextZeile Kurz0 p) = p
    zeilenText (TextZeile _ p) = p
--    zeilenText (TextZeile Text0 t) = t
    zeilenText LeerZeile     = "\n"

    renderZeile (TextZeile Zahl0 p) = ".SeitenZahl " <> p <> "page \n"
    renderZeile (TextZeile _ t) = t
    renderZeile LeerZeile     = "\n"
    renderZeile _ = ""
    -- renderZeile (TextZeile Kurz0 p) = p

checkSeitenzahlen :: [TextZeilen] -> [(Int, Int)]
checkSeitenzahlen =  diff2 . map  readSeitenzahl . filter isSeitenzahl

diff2 :: [Int] -> [(Int, Int)]
diff2 [] = []
diff2 a   =  filter (not . (1==) . snd) . zip a  $ zipWith (subtract) a (tail a)

readSeitenzahl :: TextZeilen -> Int
readSeitenzahl (TextZeile Zahl0 n) = readNoteTs ["seitenzahlread", showT n]  n
readSeitenzahl z             = errorT ["not a seitenzahl ", showT z]


markShortLines :: [TextZeilen] -> [TextZeilen]
-- ^ lines shorter  0.9 * average are marked, but less than 70 (n case we have para lines
-- recognizes poems automatically and short lines are marks for end of paragraph
-- ^ lines longer than 120 are likely paragraphs
-- missed breaks will need manual intervention
markShortLines tx = map (markOneSL limit1 limit2)  tx
    where
        (av,mx) = averageLengthTextLines tx
        limit1 =  min  70 (round (0.9 * fromIntegral av))
        limit2 = 120


markOneSL :: Int -> Int -> TextZeilen -> TextZeilen--
markOneSL limitshort limitlong (TextZeile Text0 t)
    | lengthChar t < limitshort = TextZeile Kurz0 t
    | lengthChar t > limitlong =  TextZeile Para0 t
    | otherwise                =  TextZeile Text0 t

markOneSL _ _ x = x

markAllCapsLines :: [TextZeilen] -> [TextZeilen]
markAllCapsLines = map markOneAllCaps

markOneAllCaps :: TextZeilen -> TextZeilen
markOneAllCaps tx @ (TextZeile _ t)  =
    if isCapitalizedTitle t then TextZeile AllCaps0 t else tx
markOneAllCaps x = x

isCapitalizedTitle ::  Text ->   Bool
isCapitalizedTitle =  not . any isLower . t2s

averageLengthTextLines :: [TextZeilen] -> (Int,Int)
--  compute average and max length of text lines
averageLengthTextLines etts = (1 + totChar `div` txtCt, maxChars)
    where
        txtLs = filter isTextZeile etts
        txtCt = length txtLs
        totChar = sum lengthLs
        lengthLs = map (lengthChar . zeilenText) $ txtLs
        maxChars = maximum lengthLs


--------------------
#include "MarkupText.res"
