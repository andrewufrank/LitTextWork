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

module Parser.ConvertTaggerOutput-test (convertTT
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

import Parser.ConvertTaggerOutput



englTTres = [TTdata{ttwf = "This", ttpos = "DT", ttlemma = "this"},
 TTdata{ttwf = "is", ttpos = "VBZ", ttlemma = "be"},
 TTdata{ttwf = "a", ttpos = "DT", ttlemma = "a"},
 TTdata{ttwf = "test", ttpos = "NN", ttlemma = "test"}]

test_convertTTengl = do
      let c = convertTT englRes
--      putIOwords ["result", showList' c]
      assertEqual englTTres c

germanTTRes = [TTdata{ttwf = "Ein", ttpos = "ART", ttlemma = "eine"},
 TTdata{ttwf = "anderer", ttpos = "PIAT", ttlemma = "andere"},
 TTdata{ttwf = "Satz", ttpos = "NN", ttlemma = "Satz"},
 TTdata{ttwf = ".", ttpos = "$.", ttlemma = "."}]

englRes = "This\tDT\tthis\nis\tVBZ\tbe\na\tDT\ta\ntest\tNN\ttest\n"

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
test_convertTTgerman = do
      let c = convertTT germanRes
--      putIOwords ["result", showList' c]
      assertEqual germanTTRes c

germanRes = "Ein\tART\teine\nanderer\tPIAT\tandere\nSatz\tNN\tSatz\n.\t$.\t.\n"

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
