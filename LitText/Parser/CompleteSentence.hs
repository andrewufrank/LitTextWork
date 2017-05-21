-----------------------------------------------------------------------------
--
-- Module      :  complete the sentence in Defs0 mit lemma and a second PoS
-- Copyright   : af
--
--move this code out of TT to make TT a simple server to run all the time

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables
--        , BangPatterns
         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Parser.CompleteSentence (
    completeSentence
    , htf_thisModulesTests
    )   where

import           Test.Framework
--import           Test.Invariant
import Uniform.Strings
import Uniform.Error
import Uniform.Zero
import qualified System.IO as IO
import Parser.ConvertTaggerOutput--import NLP.CallTagger2
import CoreNLP.Defs0
import CoreNLP.DependencyCodes
import Data.RDF.Extension (LanguageCode (..))
--import qualified Network.HTTP            as Net (Net.Post)
--import           Store.RDFstore.HttpCall -- (callHTTP3, parseURLchecked)

import Uniform.HttpGet

completeSentence :: Bool -> LanguageCode -> Sentence0 -> ErrIO Sentence0
completeSentence debugCS lang sent1 = do
    when debugCS $ putIOwords ["completeSentence start", showT sent1]
    let  toks = extractTokens sent1  -- not working: made strict in text to delay till text is available
                -- may resolve problem of error in accept (limit 5 caller)
    ttres <- ttProcess lang toks   -- replace by httpcall
    let tags = convertTT ttres
    let toks2 = zipWith putTags2token tags (stoks sent1)
    let sent2 = sent1{stoks = toks2}

    let sent5 = sent2
    when debugCS $ putIOwords ["completeSentence end", showT sent5]
    return sent5

ttserver = "http://127.0.0.1:17701"
ttserverTest = "http://127.0.0.1:17701/test"  -- expects blank separated tokens
ttProcess :: LanguageCode -> [Text] ->ErrIO Text
-- just the call to the server at 17701 ttscottyServer
ttProcess lang toks = do
    response <- makeHttpPost7 False ttserver [] "text/plain" (unlines' toks)
--
--    let request = makeHTTPrequest5 POST ttserver "text/plain " (unlines' toks)
--    putIOwords ["ttprocess request", s2t $ show  request]
--    response <- callHTTP7 True request
--    let response2 = bb2t response
    -- putIOwords ["ttprocess call response", showT response]
    return response



putTags2token :: TTdata -> Token0 -> Token0
-- insert the tag value to the token
putTags2token tt tok =  if (word0 . tword $ tok) == ttwf tt
                then tlemma' (const . Lemma0 . ttlemma $ tt) . tpostt' (const . ttpos $ tt) $ tok
                else errorT ["putTags2token", "not the same wordform", word0 . tword $ tok, "tagger gives", ttwf tt]

extractTokens :: Sentence0 -> [Text]
extractTokens  =   map (word0 . tword) . stoks

tlemma' f t = t{tlemma = f . tlemma $ t}
tpostt' f t = t{tpostt = f . tpostt $ t}


s0 =  Sentence0 {sid = SentID0 {unSentID0 = 1}
, sparse = "(ROOT\n  (NUR\n    (S\n      (NP (PPOSAT Unsere) (NN Namen))\n      (VAFIN werden) (ADJD lebendig))))\n\n"
    , stoks = [Token0 {tid = TokenID0 {untid0 = 1}, tword = Wordform0 {word0 = "Unsere"}
    , tlemma = Lemma0 {lemma0 = "unsere"}, tbegin = 0, tend = 6, tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}
    ,Token0 {tid = TokenID0 {untid0 = 2}, tword = Wordform0 {word0 = "Namen"}, tlemma = Lemma0 {lemma0 = "namen"}
    , tbegin = 7, tend = 12, tpos = NN, tpostt = "", tner = ["O"], tspeaker = []},Token0 {tid = TokenID0 {untid0 = 3}
    , tword = Wordform0 {word0 = "werden"}, tlemma = Lemma0 {lemma0 = "werden"}
    , tbegin = 13, tend = 19, tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}
    ,Token0 {tid = TokenID0 {untid0 = 4}, tword = Wordform0 {word0 = "lebendig"}, tlemma = Lemma0 {lemma0 = "lebendig"}
    , tbegin = 20, tend = 28, tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}]
    , sdeps = Nothing}
--    ], docCorefs = []

s9 = Sentence0{sid = SentID0{unSentID0 = 1},
          sparse =
            "(ROOT\n  (NUR\n    (S\n      (NP (PPOSAT Unsere) (NN Namen))\n      (VAFIN werden) (ADJD lebendig))))\n\n",
          stoks =
            [Token0{tid = TokenID0{untid0 = 1},
                    tword = Wordform0{word0 = "Unsere"},
                    tlemma = Lemma0{lemma0 = "unser"}, tbegin = 0, tend = 6,
                    tpos = Unk, tpostt = "PPOSAT", tner = ["O"], tspeaker = []},
             Token0{tid = TokenID0{untid0 = 2},
                    tword = Wordform0{word0 = "Namen"},
                    tlemma = Lemma0{lemma0 = "Name"}, tbegin = 7, tend = 12, tpos = NN,
                    tpostt = "NN", tner = ["O"], tspeaker = []},
             Token0{tid = TokenID0{untid0 = 3},
                    tword = Wordform0{word0 = "werden"},
                    tlemma = Lemma0{lemma0 = "werden"}, tbegin = 13, tend = 19,
                    tpos = Unk, tpostt = "VAFIN", tner = ["O"], tspeaker = []},
             Token0{tid = TokenID0{untid0 = 4},
                    tword = Wordform0{word0 = "lebendig"},
                    tlemma = Lemma0{lemma0 = "lebendig"}, tbegin = 20, tend = 28,
                    tpos = Unk, tpostt = "ADJD", tner = ["O"], tspeaker = []}],
          sdeps = Nothing}
--test_complete :: IO ()
test_complete = do
    s1 <- runErr $ completeSentence False German s0
    case s1 of
        Left msg -> errorT ["test complete", msg]
        Right s2 -> assertEqual s9 s2

{-
-- try with paragrahs
tinp =   ["<p nr=p1> <s nr=s0> Ein anderer Satz, der einfach ist.</s>"
      ,"<s nr=s1> Die Katze ist auf dem Tisch.</s>"
      ,"<s nr=s2> Der Mann geht nach hause.</s>"
      ,"</p>"] :: [Text]

resTinp = "<p nr=p1>\n<s nr=s0>\nEin\tART\teine\nanderer\tPIAT\tandere\nSatz\tNN\tSatz\n,\t$,\t,\nder\tPRELS\tdie\neinfach\tADJD\teinfach\nist\tVAFIN\tsein\n.\t$.\t.\n</s>\n<s nr=s1>\nDie\tART\tdie\nKatze\tNN\tKatze\nist\tVAFIN\tsein\nauf\tAPPR\tauf\ndem\tART\tdie\nTisch\tNN\tTisch\n.\t$.\t.\n</s>\n<s nr=s2>\nDer\tART\tdie\nMann\tNN\tMann\ngeht\tVVFIN\tgehen\nnach\tADV\tnach\nhause\tVVFIN\thausen\n.\t$.\t.\n</s>\n</p>"
        :: Text

resTTp = [TTpage "<p" "nr=p1>", TTpage "<s" "nr=s0>",
 TTdata{ttwf = "Ein", ttpos = "ART", ttlemma = "eine"},
 TTdata{ttwf = "anderer", ttpos = "PIAT", ttlemma = "andere"},
 TTdata{ttwf = "Satz", ttpos = "NN", ttlemma = "Satz"},
 TTdata{ttwf = ",", ttpos = "$,", ttlemma = ","},
 TTdata{ttwf = "der", ttpos = "PRELS", ttlemma = "die"},
 TTdata{ttwf = "einfach", ttpos = "ADJD", ttlemma = "einfach"},
 TTdata{ttwf = "ist", ttpos = "VAFIN", ttlemma = "sein"},
 TTdata{ttwf = ".", ttpos = "$.", ttlemma = "."}, TTcode "</s>",
 TTpage "<s" "nr=s1>",
 TTdata{ttwf = "Die", ttpos = "ART", ttlemma = "die"},
 TTdata{ttwf = "Katze", ttpos = "NN", ttlemma = "Katze"},
 TTdata{ttwf = "ist", ttpos = "VAFIN", ttlemma = "sein"},
 TTdata{ttwf = "auf", ttpos = "APPR", ttlemma = "auf"},
 TTdata{ttwf = "dem", ttpos = "ART", ttlemma = "die"},
 TTdata{ttwf = "Tisch", ttpos = "NN", ttlemma = "Tisch"},
 TTdata{ttwf = ".", ttpos = "$.", ttlemma = "."}, TTcode "</s>",
 TTpage "<s" "nr=s2>",
 TTdata{ttwf = "Der", ttpos = "ART", ttlemma = "die"},
 TTdata{ttwf = "Mann", ttpos = "NN", ttlemma = "Mann"},
 TTdata{ttwf = "geht", ttpos = "VVFIN", ttlemma = "gehen"},
 TTdata{ttwf = "nach", ttpos = "ADV", ttlemma = "nach"},
 TTdata{ttwf = "hause", ttpos = "VVFIN", ttlemma = "hausen"},
 TTdata{ttwf = ".", ttpos = "$.", ttlemma = "."}, TTcode "</s>",
 TTcode "</p>"]


-- testx :: ErrIO ()
-- testx = do
--           resp <- ttProcess DE (lines' tinp)
--           putIOwords ["result\n",   resp]
--           putIOwords ["result\n", showT resp]
--           putIOwords ["test eq\n", showT (resp == resTinp)]
--           let ttMulti = convertTT resp
--           putIOwords ["result\n", showList' ttMulti]
--           putIOwords ["result\n", showT ttMulti]
--           putIOwords ["test eq\n", showT (resp == resTinp)]
--
--           return ()
-- ttresult = "<p nr=1>\nEin\tART\teine\nanderer\tPIAT\tandere\n\
--   \Satz\tNN\tSatz\n,\t$,\t,\nder\tPRELS\tdie\neinfach\tADJD\teinfach\n\
--     \ist\tVAFIN\tsein\n.\t$.\t.\n</p>"  :: Text


cres = [TTpage "<p" "nr=1>",TTdata {ttwf = "Ein", ttpos = "ART", ttlemma = "eine"}
      ,TTdata {ttwf = "anderer", ttpos = "PIAT", ttlemma = "andere"}
      ,TTdata {ttwf = "Satz", ttpos = "NN", ttlemma = "Satz"}
      ,TTdata {ttwf = ",", ttpos = "$,", ttlemma = ","}
      ,TTdata {ttwf = "der", ttpos = "PRELS", ttlemma = "die"}
      ,TTdata {ttwf = "einfach", ttpos = "ADJD", ttlemma = "einfach"}
      ,TTdata {ttwf = "ist", ttpos = "VAFIN", ttlemma = "sein"}
      ,TTdata {ttwf = ".", ttpos = "$.", ttlemma = "."},TTcode "</p>"]

-}
--
--test_convertTTgerman = do
--      let c = convertTT germanRes
--      putIOwords ["result", showList' c]
--      assertEqual germanTTRes c
--
-- test_convert_mutliSentence_TT = do
--       res1 <- nlpProcess2 (t2b . unwords' $ tinp)
--       let res1t = bb2t res1
--       putIOwords ["result", showT  res1t]
--       assertEqual resTinp res1t
--       let res2 = convertTT res1t
--       putIOwords ["result", showList' res2]
--       assertEqual resTTp res2
--
{-
<p nr=1>
Ein	ART	eine
anderer	PIAT	andere
Satz	NN	Satz
,	$,	,
der	PRELS	die
einfach	ADJD	einfach
ist	VAFIN	sein
.	$.	.
</p>
<p nr=2>
Der	ART	die
Mann	NN	Mann
schrieb	VVFIN	schreiben
seinem	PPOSAT	sein
Vater	NN	Vater
,	$,	,
dass	KOUS	dass
er	PPER	er
Geld	NN	Geld
braeuchte	VVFIN	braeuchte
.	$.	.
</p>
-}
