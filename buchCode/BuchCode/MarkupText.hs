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

module BuchCode.MarkupText
    (module BuchCode.MarkupText
    , module BuchCode.BuchToken
    ) where


import           BuchCode.BuchToken hiding (try, (<|>), (</>))
import           Data.Char
import Data.Maybe  -- todo string - algebras?
import           Text.Parsec
import           Uniform.Error hiding (try, (<|>))
import           Uniform.FileIO   hiding (try, (<|>))
import           Test.Framework
import Parser.ReadMarkupAB
import Uniform.TestHarness hiding (try)

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

    renderZeile :: z -> Text -- ^ gives the literary text

type TextParsec a = Parsec Text () a
type TextParsecBuch = Parsec Text () [MarkupElement]

-- the type of the line
data TextType = Text0 | Zahl0 | Fussnote0 | Kurz0 | Para0 | AllCaps0
        deriving (Show, Read, Eq )

data TextWithMarks = TextWithMarks {twm::Text, twmMarks::[(Int, Text)]}
        deriving (Show, Read, Eq )
-- the text is without markers, the markers are a list
-- of offset of the marker from start of line resp. previous marker
-- and the marker as text
-- the list is empty if none

-- the kinds of lines differentiated
data TextZeilen =
        TextZeile {ttt::TextType, ttx::TextWithMarks}
        | MarkupZeile {ttok:: BuchToken, ttx:: TextWithMarks}
        | LeerZeile
        | NeueSeite
        deriving (Show, Read, Eq )


parseMarkup :: Text ->  [TextZeilen]  -- test B -> BA
-- parse a text file to TextZeilen form
parseMarkup  =  markShortLines
        . markAllCapsLines
            . parseMarkupText . s2t . filter (/= '\r') . t2s
-- TODO filter for char

test_0B_BA = assertEqual result0BA (parseMarkup result0B)
-- local test

-- result1A, .. result6A is exported form ReadMarkupAB.

--test_0B_BA = testFile2File "resultB0" "resultBA0" parseMarkup
test_1B_BA :: IO ()
test_1B_BA = testFile2File "resultB1" "resultBA1" parseMarkup
test_2B_BA = testFile2File "resultB2" "resultBA2" parseMarkup
test_3B_BA = testFile2File "resultB3" "resultBA3" parseMarkup
test_4B_BA = testFile2File "resultB4" "resultBA4" parseMarkup
test_5B_BA = testFile2File "resultB5" "resultBA5" parseMarkup
test_6B_BA = testFile2File "resultB6" "resultBA6" parseMarkup
test_8B_BA = testFile2File "resultB8" "resultBA8" parseMarkup



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
--    try allCapsZeile
--    <|>
    textzeileMitFussnote
--    <|>
--    try textzeile
        <?> "lineparser"


zahlzeile :: TextParsec TextZeilen
zahlzeile = do
    res <- many1  (oneOf "0123456789[]/")
                -- these are characters encountered in german textarchive
    many1 newline
    return $ TextZeile Zahl0 (TextWithMarks (s2t res) [])

fussnotezeile :: TextParsec TextZeilen
fussnotezeile = do
    fn <- fussnoteMarker
    res <- parseTextWithMarkers
    let restwm = TextWithMarks (s2t fn <> twm res) [(0,s2t fn)]
    return $ TextZeile Fussnote0  $ restwm

fussnoteMarker :: TextParsec String
fussnoteMarker = do
    res <- many1  (oneOf "0123456789[(")
    res1 <- oneOf ")]"
    return (res ++ [res1])


markupzeile :: TextParsec TextZeilen
markupzeile = do
    char '.'
    mk <- choice ( map (\t -> try (tokenElement t)) [BuchIgnoreEnd .. BuchEnde] )
    res <- parseTextWithMarkers
    return $ MarkupZeile mk  res -- $ (TextWithMarks (trim' $ s2t res) [])

--allCapsZeile  :: TextParsec TextZeilen
--allCapsZeile = do
--    res <- many (satisfy (\c -> cond c && (not $ c `elem` ['\n'])))
--    newline
--    return . TextZeile AllCaps0 $ (TextWithMarks ( s2t res) [])
--    where cond = not . isLower

textzeileMitFussnote :: TextParsec TextZeilen
textzeileMitFussnote = do
    res <- parseTextWithMarkers
    return . TextZeile Text0 $  res

pair (f,g) (a,b) = (f a, g b)
-- todo algebras

parseTextWithMarkers :: TextParsec TextWithMarks
parseTextWithMarkers = do
    res1 <- many (noneOf "\n[")  -- symbol for marks
    choice
        [
          try $ do -- found a marker has perhaps more markers
                    fn <- fussnoteMarker
                    let resx1 = [(res1, fn)]
                    resx3 <- optionMaybe (try annotherFootnoteMarker)
                    res4 <- many (noneOf "\n")
                    let resx4 = [(res4, "")]
                    let ress = concat [resx1, fromMaybe [] resx3, resx4]
                    newline
                    let res14 = trim' . s2t .concat . map fst $ ress
                    let list = map (pair (lengthChar, s2t)) ress
                    return $ TextWithMarks  res14  list

         ,  do -- found a [ but not marker
            res2 <- many (noneOf "\n")
            newline
            let res12 = res1 ++ res2
            return $ TextWithMarks (trim' . s2t $ res12) []
      ]

annotherFootnoteMarker :: TextParsec [(String,  String)]
annotherFootnoteMarker = do
        res3 <- many (noneOf "\n[(")
        fn2 <-  fussnoteMarker
        xres <- optionMaybe $ try annotherFootnoteMarker
        return $ [(res3,  fn2)] ++ fromMaybe [] xres

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

    zeilenText (TextZeile _ p) = twm p
    zeilenText LeerZeile     = "\n"

    renderZeile (TextZeile Zahl0 p) = ".SeitenZahl " <> twm p <> "page \n"
    renderZeile (TextZeile _ t) = twm t
    renderZeile LeerZeile     = "\n"
    renderZeile _ = ""
    -- renderZeile (TextZeile Kurz0 p) = p

checkSeitenzahlen :: [TextZeilen] -> [(Int, Int)]
checkSeitenzahlen =  diff2 . map  readSeitenzahl . filter isSeitenzahl

diff2 :: [Int] -> [(Int, Int)]
diff2 [] = []
diff2 a   =  filter (not . (1==) . snd) . zip a  $ zipWith (subtract) a (tail a)

readSeitenzahl :: TextZeilen -> Int
readSeitenzahl (TextZeile Zahl0 n) = readNoteTs ["seitenzahlread", showT n]  (twm n)
readSeitenzahl z             = errorT ["not a seitenzahl ", showT z]


markShortLines :: [TextZeilen] -> [TextZeilen]
-- ^ lines shorter  0.9 * average are marked, but less than 70 (n case we have para lines
-- recognizes poems automatically and short lines are marks for end of paragraph
-- ^ lines longer than 120 are likely paragraphs
-- missed breaks will need manual intervention
markShortLines tx = map (markOneSL limit1 limit2)  tx
    where
        (av,mx) = averageLengthTextLines tx
        limit1 =  max 20 $ min  70 (round (0.9 * fromIntegral av))
        limit2 = 120


markOneSL :: Int -> Int -> TextZeilen -> TextZeilen--
markOneSL limitshort limitlong (TextZeile Text0 t)
    | lengthChar txt < limitshort = TextZeile Kurz0 t
    | lengthChar txt > limitlong =  TextZeile Para0 t
    | otherwise                =  TextZeile Text0 t
  where txt = twm t

markOneSL _ _ x = x

markAllCapsLines :: [TextZeilen] -> [TextZeilen]
markAllCapsLines = map markOneAllCaps

markOneAllCaps :: TextZeilen -> TextZeilen
markOneAllCaps tz @ (TextZeile Zahl0 t) = tz
markOneAllCaps tx @ (TextZeile _ t)  =
    if isCapitalizedTitle (twm t) then TextZeile AllCaps0 t else tx
--    if isCapitalizedTitle (twm t) then MarkupZeile BuchHL2 t else tx
    -- is this a clever idea? hl2 for all caps
    -- how to allow a switch?
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
result0B = unlines'  ["wort1;langeswort2"
            ,"55"
            ,".sprache German"
            ,""
            ,"1960 is a good"
            ,"eine kurze [vielleicht wichtige] zeile"
            ,"66"
            , "als\n\f\n[54/0002]\nda\223 man ihm erstens,"
            , "als\r\n\f\r\n[54/0002]\r\nda\223 man[2] ihm [1] mit Fussnoten,"
            ,""
            ,".titel TIT [3]"
            ,"zweite,[4] [5][6] kurze zeile[3]"
            ,"II.--THE COUNCIL HELD BY THE RATS [4]"
            ,"   Old Rodilard,[5] a certain cat,"
            ,"II - ALL CAPS TEST"
            , "[44]"  -- seitenzahl
            , "[1] eine Fussnote"
            ,"77"] ::  Text

result0BA =

    [TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "wort1;langeswort2", twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "55", twmMarks = []}},
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "German", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "1960 is a good", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "eine kurze [vielleicht wichtige] zeile",
                               twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "66", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "als", twmMarks = []}},
     NeueSeite,
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "[54/0002]", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "da\223 man ihm erstens,", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "als", twmMarks = []}},
     NeueSeite,
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "[54/0002]", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "da\223 man ihm  mit Fussnoten,",
                               twmMarks = [(7, "[2]"), (5, "[1]"), (15, "")]}},
     LeerZeile,
     MarkupZeile{ttok = BuchTitel,
                 ttx =
                   TextWithMarks{twm = "TIT", twmMarks = [(5, "[3]"), (0, "")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "zweite,  kurze zeile",
                               twmMarks =
                                 [(7, "[4]"), (1, "[5]"), (0, "[6]"), (12, "[3]"), (0, "")]}},
     TextZeile{ttt = AllCaps0,
               ttx =
                 TextWithMarks{twm = "II.--THE COUNCIL HELD BY THE RATS",
                               twmMarks = [(34, "[4]"), (0, "")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "Old Rodilard, a certain cat,",
                               twmMarks = [(16, "[5]"), (15, "")]}},
     TextZeile{ttt = AllCaps0,
               ttx = TextWithMarks{twm = "II - ALL CAPS TEST", twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "[44]", twmMarks = []}},
     TextZeile{ttt = Fussnote0,
               ttx =
                 TextWithMarks{twm = "[1]eine Fussnote", twmMarks = [(0, "[1]")]}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "77", twmMarks = []}}]
