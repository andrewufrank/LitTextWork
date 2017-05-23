---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the reading the markup files and
-- removing characters not pertaining
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ReadMarkupAB
    (module Parser.ReadMarkupAB
        ) where

import           Test.Framework

import           Parser.Foundation        hiding ((</>), (<.>))
import          Producer.Servers
import           Uniform.FileIO
--import           Uniform.Strings  hiding ((</>), (<.>))
-- import           BuchCode.MarkupText (parseMarkup, result1B, result2B, result3B, result4B)

--  the inputs for the tests

--testEndpoint = "http://127.0.0.1:3030/testDB/update"
testDir = makeAbsDir ("/home/frank/additionalSpace/DataBig/LitTest")
serverLocTest = serverBrest --
--serverLocTest = serverLocalhost


data Markup
-- just a marking for a file type

markupFileType5 = mkTypedFile5 :: TypedFile5 Text Markup

instance TypedFiles5 Text Markup  where
    -- files of a single text stream, with a markup extension
    mkTypedFile5  = TypedFile5 { tpext5 = Extension "markup"
                    -- , parserF = tparser
                    -- , writerF = twriter
            }
    write5 fp fn tp  ct = do
        dirx <- ensureDir fp
        let fn2 = fn <.> tpext5 tp -- :: Path ar File
        writeFile2 (fp </> fn2 ) ct
    read5 fp fn tp   = do
        let fn2 = fn <.> (tpext5 tp)
        readFile2 (fp </> fn2)
--    write5 fp fn tp   = writeFileOrCreate (combineFilepath fp fn (tpext5 tp))
----                (fp </> (fn <.> (tpext tp) )) a
--    read5 fp fn tp   = readFile2 $ combineFilepath fp fn (tpext5 tp)
----                (fp </> (fn <.> (tpext tp) ))

--- the A_B code (could go to separate file)

debugRead = False
-- main export
textstate2Text :: TextState2 -> ErrIO Text  -- test A -> B
-- reads the markup file and converts to coded llines
textstate2Text textstate = do
    t <- _readMarkupFile textstate -- test A -> B
    when debugRead $ putIOwords ["textstate2TZ - the file content\n", t,
                         "\n with show\n", showT t, "\nend of file"]
    let t2 = filterChar (`notElem` ['\r']) t
    return t2

_readMarkupFile :: TextState2 -> ErrIO Text
_readMarkupFile textstate = do
    text <-  read5 ((originalsDir textstate) </>
                (makeRelDir .  authorDir $ textstate) :: Path Abs Dir)

            (makeRelFile .  buchname $ textstate) markupFileType5
    bomWarning text   -- the check for BOM is in MainParse only -
    let nonlats =  nubChar . findNonLatinCharsT $ text
    putIOwords ["this file contains characters not in the latin1 charset which are not yet mapped\n"
                 , nonlats, showT nonlats]
    return text

bomWarning v = do  -- not required - parser filter it out
    -- putIOwords ["bomWarning - the start of the file "]
    -- putIOwords [take' 20 v]
    -- putIOwords [take' 20 $ showT v]
    if isPrefixOf' "\65279" v
        then putIOwords ["WARNING -- BOM character present - use ./unbom"]
        else putIOwords ["file start is ok"]
    return ()

test_CR :: IO ()
test_CR = assertEqual (filterChar (`notElem` ['\r']) ins) outs

  where
   ins, outs :: Text
   ins = ".sprache German\r\n.isbn ISBN -8\r\n.author Yoko Tawada\r\n.titel"
   outs = ".sprache German\n.isbn ISBN -8\n.author Yoko Tawada\n.titel"
------------------------- tests A -> B
test_1_A_B_textstate_text_1 :: IO ()
-- ^ test for the conversion from textstate to text (including markup, but not decoded)
-- the textResult is in LinesToParagraphs
--
test_1_A_B_textstate_text_1 = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    t1 <- runErr $  textstate2Text result1A
    case t1 of
        Left msg -> do
                    putIOwords ["test_textstate_text failed", msg]
                    assertBool False
        Right tt1 -> do
--                    putIOwords ["the text result (for next) \n", showT tt1]
--                    putIOwords ["the text result   \n",   tt1]
                    assertEqual result1B tt1

test_2_A_B_textstate_text_1 :: IO ()
-- ^ test for the conversion from textstate to text (including markup, but not decoded)
-- the textResult is in LinesToParagraphs
--  this test is specific for very long lines which are paragraphs
test_2_A_B_textstate_text_1 = do
    t1 <- runErr $  textstate2Text result2A
    assertEqual (Right result2B)  t1
test_3_A_B_textstate_text_1 = do
    t1 <- runErr $  textstate2Text result3A
    assertEqual (Right result3B)  t1
test_4_A_B_textstate_text_1 = do
    t1 <- runErr $  textstate2Text result4A
    assertEqual (Right result4B)  t1
test_5_A_B_textstate_text_1 = do
    t1 <- runErr $  textstate2Text result5A
    assertEqual (Right result5B)  t1
test_6_A_B_textstate_text_1 = do
    t1 <- runErr $  textstate2Text result6A
    assertEqual (Right result6B)  t1

result1A = TextState2 { --endpoint = testEndpoint,
    serverLoc = serverLocTest
    , originalsDir =  testDir
    , authorDir = "test", buchname = "t1"}

result2A = TextState2 { -- endpoint = testEndpoint,
    serverLoc = serverLocTest
    , originalsDir =  testDir
    , authorDir = "test", buchname = "t2"}

result3A = TextState2 { --endpoint = testEndpoint,
    serverLoc = serverLocTest
    , originalsDir = testDir
    , authorDir = "test", buchname = "t3"}

result4A = TextState2 { --endpoint = testEndpoint,
    serverLoc = serverLocTest
    , originalsDir = testDir
    , authorDir = "test", buchname = "t4"}

result5A = TextState2 { --endpoint = testEndpoint,
    serverLoc = serverLocTest
    , originalsDir = testDir
    , authorDir = "test", buchname = "t5"}

result6A = TextState2 { --endpoint = testEndpoint,
    serverLoc = serverLocTest
    , originalsDir = testDir
    , authorDir = "test", buchname = "t6"}

result1B  =
        "\n.sprache German\n\n.author PETER WATERHOUSE\n\n.titel (Krieg f\252r Welt)\n11\n\n.hl1 Unsere Br\228uche werden lebendig\n12\nWas w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?\n\nEr fragte sich als zweiter Paragraph.\n13\n\n.ende\n"

result2B =
    "\n.sprache German\n\n.author PETER WATERHOUSE\n\n.titel (Krieg f\252r Welt) - als test fuer paragraph per zeile\n11\n\n.hl1 Unsere Br\228uche werden lebendig\n12\nWas w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?\n13\n\nEs war einmal ein Dorf in einem Tal, in dem kein Reis wuchs. Eine schwangere Frau hatte einen Fisch gefunden und verzehrte ihn, weil sie hungrig war, roh und ohne daran zu denken, ihn mit den anderen im Dorf zu teilen. Sie gebar einen sch\246nen Sohn. Sp\228ter aber wuchsen auf ihrem K\246rper Schuppen und sie wurde zu einem gro\223en Fisch. Weil sie zu Lande nicht mehr wohnen konnte, f\252hrte sie fortan ein einsames Leben im Fluss. Ihr Sohn wurde von einem Alten im Dorf aufgezogen. Zu allen Zeiten verh\246hnen die Knaben die M\252tter der anderen im Streit. Sie streiten, ohne zu verstehen, was sie sagen: \187Deine Mutter ist eine Hure!\171\nIch wusch mein Gesicht mit wei\223em Sand. Nur so konnte ich meine Haut, die zu einer W\252ste geworden war, wieder glatt bekommen. Man sagt, dass dieser Sand von den Knochen eines Dinosauriers stamme, von Knochen, die die Wellen des Meeres lange gewaschen haben und die die Sonne getrocknet hat. Ich verteilte ihn auf meine Handfl\228chen und legte diese auf mein Gesicht; sie fingen durch das Fleisch hindurch mit meinen Knochen ein Gespr\228ch an. Ich konnte die Form meines Sch\228dels in meinen H\228nden genau sp\252ren. Au\223er der aus Licht gewordenen Haut und des aus Wasser gewordenen Fleisches gibt es noch einen K\246rper. Aber solange ich lebe, kann niemand diesen K\246rper umarmen.\n.ende\n"

result3B =  "\n.sprache German\n\n\n.titel test fuer paragraphen mit mehreren zeilen\n.ignore Der Autor dankt dem Deutschen Literaturfonds f\252r die Unterst\252tzung.\n.copyright \169 2006 Jung und Jung, Salzburg und Wien\n.ignore Satz: Media Design: Riznerat, Salzburg\n.ignore Druck: Friedrich Pustet, Regensburg\n.isbn ISBN 3-902497-13-0, 978-3-902497-13-0\n\n\n\n,author PETER WATERHOUSE\n.ignore (Krieg und Welt)\n\n.verlag JUNG UND JUNG\n\n\n\n\n\n.hl1 Unsere Namen werden lebendig\nWas ist ihm fremd und was sein eigen?\nIch ging aus dem Dorf und tr\228umte vor mich hin einen Satz, oder es\nwaren ein paar Worte oder Worte aus Worten kommend, eine halbe\nMelodie. Das Dorf bald tausend Meter zur\252ck, das W\228ldchen eine halbe\nMeile hinter Wiese und Wiese: Das Dorf tanzte heran und war wie mit\nder Hand ber\252hrbar und das W\228ldchen gleichwohl ein Wegbegleiter und\ndie vielen Wiesen ein einziges Hemd. Gerettet. Wovor das Dorf gerettet?\nEin Engel legt mir die H\228nde auf den K\246rper? Wald eine Kinderschar?\nAlle meine Kinder? Mehr als ein Vater, Land aller V\228ter; stehen da auf\nder leeren Wiese die V\228ter?\nIch stand. Wie standen die? Ich ging wenige Schritte und sagte mir hin:\nIch werde geschoben. Stand. Ich werde gehoben. Es war still. Ich werde\ngeh\246rt. Ich h\246rte. Ich werde geh\246rt. Es war kein Haus da. Es ist kein\nHaus da. Ein Strauch rauschte. Er rauscht, so sagte ich es mir. Es war in\ndiesem Aus-dem-Dorf-Gehen kein Vergehen. Gar kein Leichenbegr\228bnis.\nWarum ziehen hier keine Leichenz\252ge? Wo sind die Toten? Wo hingelegt?\nDer Ruf auf der Dorfstra\223e - den ich jetzt h\246rte - wird f\252nfzig und mehr\nJahre durchrufen. Der Ruf ist ein Mensch, der lang ist. Ich nehme den\nDorfruf mit wie mich selbst, hab ihn wie ein Auge und Ohr und wie\nFinger so viel. Ich \246ffnete die Hand, hielt sie ge\246ffnet vor mich hin, die\n51\n\n\nInnenfl\228che aufw\228rts w\252st und offen. Was w\252rde mit der Hand gesche-\nhen? Ich schaute sie lange an. Um die Hand eine w\252ste, wei\223e Welt.\n\nMeine Hand war rot.\nUnd wendete mich und schaute. Da war das Dorf. Es war braun und\nrot. Es war etwas, ein Fleck, eine dunkle Frucht. Es rauchte aus Stellen\nder Frucht. Das Dorf schwebte an einer Art von Asth\246he und in einer Art\nund Weise. Tiefer als das Dorf waren Wiesen, das Gr\252ne. Das Dorf jetzt\nnicht viel gr\246\223er als meine Hand. Fast ein Auge.\n\n.ende\n\n\n\n\n\n\n\n\n"

result4B =
  ".sprache German\n.isbn ISBN 3-88769-324-8\n.author Yoko Tawada\n.titel \
  \Das nackte Auge\n.verlag konkursbuch verlag claudia gehrke\n.dedication f\252r C.D.\n6\n.hl1 \
  \Kapitel 1 REPULSION\nEin gefilmtes Auge, angeheftet an einem bewusstlosen K\246rper. Es sieht nichts, \
  \denn die Kamera hat ihm schon die Sehkraft geraubt.  Er hatte uns mehrmals von seinem Aufenthalt in \
  \Berlin und einem gewissen \"Pergamonmuseum\" erz\228hlt. \"Pergamon\"\n7\nklang wie der Name \
  \eines Wandervogels, und uns gefiel die Vorstellung des Berliner Himmels, in dem dieser Vogel flatterte.  \
  \Einer\n8\nvon ihnen nahm mir meine Reisetasche ab. Was sollte man einem solchen Manne schreiben, \
  \der sich offenbar verrannt hatte. Sollte man ihm vielleicht raten, wieder nach Hause zu kommen, \
  \seine Existenz hierher zu verlegen, alle die alten freundschaftlichen Beziehungen wieder \
  \aufzunehmen \8212 wof\252r ja kein Hindernis bestand \8212 und im \252brigen auf die Hilfe der Freunde \
  \zu vertrauen? Das bedeutete aber nichts anderes, als\n\f\n[54/0002]\nda\223 man ihm gleichzeitig, \
  \je schonender, desto kr\228nkender, sagte, da\223 seine bisherigen Versuche mi\223lungen seien, \
  \da\223 er endlich von ihnen ablassen solle, da\223 er zur\252ckkehren und sich als ein f\252r immer \
  \Zur\252ckgekehrter von allen mit gro\223en Augen anstaunen lassen m\252sse, da\223 nur seine Freunde \
  \etwas verst\252nden und da\223 er ein altes Kind sei, das den erfolgreichen, zu Hause gebliebenen \
  \Freunden einfach zu folgen habe.Er schien etwas erschrocken zu sein, wahrscheinlich, weil sie unerwartet \
  \leicht war. \n.ende\n"

result5B =  ".Title: The Fables of La Fontaine\n.ignore        A New Edition, with notes\n\n.Author: Jean de la Fontaine\n\n.Language: English\n\nPREFACE\n\nTo The Present Edition,\n\n\n\nII.--THE COUNCIL HELD BY THE RATS [4]\n\n    Old Rodilard,[5] a certain cat,\n      Such havoc of the rats had made,\n    'Twas difficult to find a rat\n      With nature's debt unpaid.\n\n \n[4] Faerno and Abstemius both have fables upon this subject. Gabriel\n    Faerno (1500-1561) was an Italian writer who published fables in\n    Latin. Perrault translated these into French verse, and published\n    them at Paris in 1699.\n[5] _Rodilard_.--The name no doubt taken from the famous cat\n    Rodilardus (bacon-gnawer), in Rabelais, _Pantagruel_, IV., ch. LXVII.\n\nIV.--THE TWO BULLS AND THE FROG.[9]\n\n  Two bulls engaged in shocking battle,\n    Both for a certain heifer's sake,\n  And lordship over certain cattle,\n    A frog began to groan and quake.\n\n  One bull was beat, and much to their expense;\n  For, quick retreating to their reedy bower,\n  He trod on twenty of them in an hour.\n\n    Of little folks it oft has been the fate\n    To suffer for the follies of the great.\n\n[9] Phaedrus, I. 30.\n\n.ende\n\n"

result6B =  ".ignoreto\nProject Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll\n\ntest for ignore etc.\n\n\nPosting Date: June 25, 2008 [EBook #11]\nRelease Date: March, 1994\n[Last updated: December 20, 2011]\n\n.Language: English\n\n\n*** START OF THIS PROJECT GUTENBERG EBOOK ALICE'S ADVENTURES IN WONDERLAND ***\n\n.ignoreend\n\n\n.sprache English\n\n\n\n\n\n.titel ALICE'S ADVENTURES IN WONDERLAND\n\n.author Lewis Carroll\n\n.publicationDetail THE MILLENNIUM FULCRUM EDITION 3.0\n\n\n.hl1 CHAPTER I. Down the Rabbit-Hole\n\nAlice was beginning to get very tired of sitting by her sister on the\nbank.\n\n.sprache Deutsch\n\nEin deutscher Satz.\n\n.sprache English \n\nThere was nothing so VERY remarkable in that.\n\nIn another moment down went Alice after it, never once considering how\nin the world she was to get out again.\n\n\n.gedicht\n         'Fury said to a\n         mouse, That he\n        met in the\n       house.\n\n.hl1 CHAPTER IV. The Rabbit Sends in a Little Bill\n\nIt was the White Rabbit, trotting slowly back again, and looking\nanxiously about as it went, as if it had lost something .\n\n.ignore              THE END\n\n.ignoreTo\n\nEnd of Project Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll\n\n\n.ignoreend \n\n.ende\n"

