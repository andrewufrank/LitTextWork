-- result0B, result1B, result2B, result3B, result4B, result5B :: Text
-- these are the input files, replacing what is read from disk
-- and are now imported from ReadMarkup

result0B = unlines'  ["wort1;langeswort2"
            ,"55"
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

result1BA =

    [LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "German", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchAuthor,
                 ttx = TextWithMarks{twm = "PETER WATERHOUSE", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchTitel,
                 ttx = TextWithMarks{twm = "(Krieg f\252r Welt)", twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "11", twmMarks = []}},
     MarkupZeile{ttok = BuchHL1,
                 ttx =
                   TextWithMarks{twm = "Unsere Br\228uche werden lebendig",
                                 twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "12", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "Er fragte sich als zweiter Paragraph.",
                               twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "13", twmMarks = []}},
     MarkupZeile{ttok = BuchEnde,
                 ttx = TextWithMarks{twm = "", twmMarks = []}}]


result2BA =    [LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "German", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchAuthor,
                 ttx = TextWithMarks{twm = "PETER WATERHOUSE", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchTitel,
                 ttx =
                   TextWithMarks{twm =
                                   "(Krieg f\252r Welt) - als test fuer paragraph per zeile",
                                 twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "11", twmMarks = []}},
     MarkupZeile{ttok = BuchHL1,
                 ttx =
                   TextWithMarks{twm = "Unsere Br\228uche werden lebendig",
                                 twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "12", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm =
                                 "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
                               twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "13", twmMarks = []}},
     TextZeile{ttt = Para0,
               ttx =
                 TextWithMarks{twm =
                                 "Es war einmal ein Dorf in einem Tal, in dem kein Reis wuchs. Eine schwangere Frau hatte einen Fisch gefunden und verzehrte ihn, weil sie hungrig war, roh und ohne daran zu denken, ihn mit den anderen im Dorf zu teilen. Sie gebar einen sch\246nen Sohn. Sp\228ter aber wuchsen auf ihrem K\246rper Schuppen und sie wurde zu einem gro\223en Fisch. Weil sie zu Lande nicht mehr wohnen konnte, f\252hrte sie fortan ein einsames Leben im Fluss. Ihr Sohn wurde von einem Alten im Dorf aufgezogen. Zu allen Zeiten verh\246hnen die Knaben die M\252tter der anderen im Streit. Sie streiten, ohne zu verstehen, was sie sagen: \187Deine Mutter ist eine Hure!\171",
                               twmMarks = []}},
     TextZeile{ttt = Para0,
               ttx =
                 TextWithMarks{twm =
                                 "Ich wusch mein Gesicht mit wei\223em Sand. Nur so konnte ich meine Haut, die zu einer W\252ste geworden war, wieder glatt bekommen. Man sagt, dass dieser Sand von den Knochen eines Dinosauriers stamme, von Knochen, die die Wellen des Meeres lange gewaschen haben und die die Sonne getrocknet hat. Ich verteilte ihn auf meine Handfl\228chen und legte diese auf mein Gesicht; sie fingen durch das Fleisch hindurch mit meinen Knochen ein Gespr\228ch an. Ich konnte die Form meines Sch\228dels in meinen H\228nden genau sp\252ren. Au\223er der aus Licht gewordenen Haut und des aus Wasser gewordenen Fleisches gibt es noch einen K\246rper. Aber solange ich lebe, kann niemand diesen K\246rper umarmen.",
                               twmMarks = []}},
     MarkupZeile{ttok = BuchEnde,
                 ttx = TextWithMarks{twm = "", twmMarks = []}}]

result3BA =

    [LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "German", twmMarks = []}},
     LeerZeile, LeerZeile,
     MarkupZeile{ttok = BuchTitel,
                 ttx =
                   TextWithMarks{twm = "test fuer paragraphen mit mehreren zeilen",
                                 twmMarks = []}},
     MarkupZeile{ttok = BuchIgnore,
                 ttx =
                   TextWithMarks{twm =
                                   "Der Autor dankt dem Deutschen Literaturfonds f\252r die Unterst\252tzung.",
                                 twmMarks = []}},
     MarkupZeile{ttok = BuchCopyright,
                 ttx =
                   TextWithMarks{twm = "\169 2006 Jung und Jung, Salzburg und Wien",
                                 twmMarks = []}},
     MarkupZeile{ttok = BuchIgnore,
                 ttx =
                   TextWithMarks{twm = "Satz: Media Design: Riznerat, Salzburg",
                                 twmMarks = []}},
     MarkupZeile{ttok = BuchIgnore,
                 ttx =
                   TextWithMarks{twm = "Druck: Friedrich Pustet, Regensburg",
                                 twmMarks = []}},
     MarkupZeile{ttok = BuchISBN,
                 ttx =
                   TextWithMarks{twm = "ISBN 3-902497-13-0, 978-3-902497-13-0",
                                 twmMarks = []}},
     LeerZeile, LeerZeile, LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = ",author PETER WATERHOUSE", twmMarks = []}},
     MarkupZeile{ttok = BuchIgnore,
                 ttx = TextWithMarks{twm = "(Krieg und Welt)", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchVerlag,
                 ttx = TextWithMarks{twm = "JUNG UND JUNG", twmMarks = []}},
     LeerZeile, LeerZeile, LeerZeile, LeerZeile, LeerZeile,
     MarkupZeile{ttok = BuchHL1,
                 ttx =
                   TextWithMarks{twm = "Unsere Namen werden lebendig",
                                 twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "Was ist ihm fremd und was sein eigen?",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Ich ging aus dem Dorf und tr\228umte vor mich hin einen Satz, oder es",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "waren ein paar Worte oder Worte aus Worten kommend, eine halbe",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Melodie. Das Dorf bald tausend Meter zur\252ck, das W\228ldchen eine halbe",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Meile hinter Wiese und Wiese: Das Dorf tanzte heran und war wie mit",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "der Hand ber\252hrbar und das W\228ldchen gleichwohl ein Wegbegleiter und",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "die vielen Wiesen ein einziges Hemd. Gerettet. Wovor das Dorf gerettet?",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Ein Engel legt mir die H\228nde auf den K\246rper? Wald eine Kinderschar?",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Alle meine Kinder? Mehr als ein Vater, Land aller V\228ter; stehen da auf",
                               twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "der leeren Wiese die V\228ter?",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Ich stand. Wie standen die? Ich ging wenige Schritte und sagte mir hin:",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Ich werde geschoben. Stand. Ich werde gehoben. Es war still. Ich werde",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "geh\246rt. Ich h\246rte. Ich werde geh\246rt. Es war kein Haus da. Es ist kein",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Haus da. Ein Strauch rauschte. Er rauscht, so sagte ich es mir. Es war in",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "diesem Aus-dem-Dorf-Gehen kein Vergehen. Gar kein Leichenbegr\228bnis.",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Warum ziehen hier keine Leichenz\252ge? Wo sind die Toten? Wo hingelegt?",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Der Ruf auf der Dorfstra\223e - den ich jetzt h\246rte - wird f\252nfzig und mehr",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Jahre durchrufen. Der Ruf ist ein Mensch, der lang ist. Ich nehme den",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Dorfruf mit wie mich selbst, hab ihn wie ein Auge und Ohr und wie",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Finger so viel. Ich \246ffnete die Hand, hielt sie ge\246ffnet vor mich hin, die",
                               twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "51", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Innenfl\228che aufw\228rts w\252st und offen. Was w\252rde mit der Hand gesche-",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "hen? Ich schaute sie lange an. Um die Hand eine w\252ste, wei\223e Welt.",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "Meine Hand war rot.", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Und wendete mich und schaute. Da war das Dorf. Es war braun und",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "rot. Es war etwas, ein Fleck, eine dunkle Frucht. Es rauchte aus Stellen",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "der Frucht. Das Dorf schwebte an einer Art von Asth\246he und in einer Art",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "und Weise. Tiefer als das Dorf waren Wiesen, das Gr\252ne. Das Dorf jetzt",
                               twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm =
                                 "nicht viel gr\246\223er als meine Hand. Fast ein Auge.",
                               twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchEnde,
                 ttx = TextWithMarks{twm = "", twmMarks = []}},
     LeerZeile, LeerZeile, LeerZeile, LeerZeile, LeerZeile, LeerZeile,
     LeerZeile, LeerZeile]


result4BA =


    [MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "German", twmMarks = []}},
     MarkupZeile{ttok = BuchISBN,
                 ttx = TextWithMarks{twm = "ISBN 3-88769-324-8", twmMarks = []}},
     MarkupZeile{ttok = BuchAuthor,
                 ttx = TextWithMarks{twm = "Yoko Tawada", twmMarks = []}},
     MarkupZeile{ttok = BuchTitel,
                 ttx = TextWithMarks{twm = "Das nackte Auge", twmMarks = []}},
     MarkupZeile{ttok = BuchVerlag,
                 ttx =
                   TextWithMarks{twm = "konkursbuch verlag claudia gehrke",
                                 twmMarks = []}},
     MarkupZeile{ttok = BuchDedikation,
                 ttx = TextWithMarks{twm = "f\252r C.D.", twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "6", twmMarks = []}},
     MarkupZeile{ttok = BuchHL1,
                 ttx = TextWithMarks{twm = "Kapitel 1 REPULSION", twmMarks = []}},
     TextZeile{ttt = Para0,
               ttx =
                 TextWithMarks{twm =
                                 "Ein gefilmtes Auge, angeheftet an einem bewusstlosen K\246rper. Es sieht nichts, denn die Kamera hat ihm schon die Sehkraft geraubt.  Er hatte uns mehrmals von seinem Aufenthalt in Berlin und einem gewissen \"Pergamonmuseum\" erz\228hlt. \"Pergamon\"",
                               twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "7", twmMarks = []}},
     TextZeile{ttt = Para0,
               ttx =
                 TextWithMarks{twm =
                                 "klang wie der Name eines Wandervogels, und uns gefiel die Vorstellung des Berliner Himmels, in dem dieser Vogel flatterte.  Einer",
                               twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "8", twmMarks = []}},
     TextZeile{ttt = Para0,
               ttx =
                 TextWithMarks{twm =
                                 "von ihnen nahm mir meine Reisetasche ab. Was sollte man einem solchen Manne schreiben, der sich offenbar verrannt hatte. Sollte man ihm vielleicht raten, wieder nach Hause zu kommen, seine Existenz hierher zu verlegen, alle die alten freundschaftlichen Beziehungen wieder aufzunehmen \8212 wof\252r ja kein Hindernis bestand \8212 und im \252brigen auf die Hilfe der Freunde zu vertrauen? Das bedeutete aber nichts anderes, als",
                               twmMarks = []}},
     NeueSeite,
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "[54/0002]", twmMarks = []}},
     TextZeile{ttt = Para0,
               ttx =
                 TextWithMarks{twm =
                                 "da\223 man ihm gleichzeitig, je schonender, desto kr\228nkender, sagte, da\223 seine bisherigen Versuche mi\223lungen seien, da\223 er endlich von ihnen ablassen solle, da\223 er zur\252ckkehren und sich als ein f\252r immer Zur\252ckgekehrter von allen mit gro\223en Augen anstaunen lassen m\252sse, da\223 nur seine Freunde etwas verst\252nden und da\223 er ein altes Kind sei, das den erfolgreichen, zu Hause gebliebenen Freunden einfach zu folgen habe.Er schien etwas erschrocken zu sein, wahrscheinlich, weil sie unerwartet leicht war.",
                               twmMarks = []}},
     MarkupZeile{ttok = BuchEnde,
                 ttx = TextWithMarks{twm = "", twmMarks = []}}]

result5BA =

    [MarkupZeile{ttok = BuchTitel,
                 ttx =
                   TextWithMarks{twm = "The Fables of La Fontaine", twmMarks = []}},
     MarkupZeile{ttok = BuchIgnore,
                 ttx =
                   TextWithMarks{twm = "A New Edition, with notes", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchAuthor,
                 ttx = TextWithMarks{twm = ": Jean de la Fontaine", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "English", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = AllCaps0,
               ttx = TextWithMarks{twm = "PREFACE", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "To The Present Edition,", twmMarks = []}},
     LeerZeile, LeerZeile, LeerZeile,
     TextZeile{ttt = AllCaps0,
               ttx =
                 TextWithMarks{twm = "II.--THE COUNCIL HELD BY THE RATS",
                               twmMarks = [(34, "[4]"), (0, "")]}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "Old Rodilard, a certain cat,",
                               twmMarks = [(17, "[5]"), (15, "")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "Such havoc of the rats had made,",
                               twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "'Twas difficult to find a rat",
                               twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "With nature's debt unpaid.", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = AllCaps0,
               ttx = TextWithMarks{twm = "", twmMarks = []}},
     TextZeile{ttt = Fussnote0,
               ttx =
                 TextWithMarks{twm =
                                 "[4]Faerno and Abstemius both have fables upon this subject. Gabriel",
                               twmMarks = [(0, "[4]")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Faerno (1500-1561) was an Italian writer who published fables in",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Latin. Perrault translated these into French verse, and published",
                               twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "them at Paris in 1699.", twmMarks = []}},
     TextZeile{ttt = Fussnote0,
               ttx =
                 TextWithMarks{twm =
                                 "[5]_Rodilard_.--The name no doubt taken from the famous cat",
                               twmMarks = [(0, "[5]")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Rodilardus (bacon-gnawer), in Rabelais, _Pantagruel_, IV., ch. LXVII.",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = AllCaps0,
               ttx =
                 TextWithMarks{twm = "IV.--THE TWO BULLS AND THE FROG.",
                               twmMarks = [(32, "[9]"), (0, "")]}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "Two bulls engaged in shocking battle,",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "Both for a certain heifer's sake,",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "And lordship over certain cattle,",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "A frog began to groan and quake.",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "One bull was beat, and much to their expense;",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "For, quick retreating to their reedy bower,",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "He trod on twenty of them in an hour.",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "Of little folks it oft has been the fate",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "To suffer for the follies of the great.",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Fussnote0,
               ttx =
                 TextWithMarks{twm = "[9]Phaedrus, I. 30.",
                               twmMarks = [(0, "[9]")]}},
     LeerZeile,
     MarkupZeile{ttok = BuchEnde,
                 ttx = TextWithMarks{twm = "", twmMarks = []}},
     LeerZeile]

result6BA =
    [MarkupZeile{ttok = BuchIgnoreTo,
                 ttx = TextWithMarks{twm = "", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Project Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "test for ignore etc.", twmMarks = []}},
     LeerZeile, LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "Posting Date: June 25, 2008 [EBook #11]",
                               twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "Release Date: March, 1994", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx =
                 TextWithMarks{twm = "[Last updated: December 20, 2011]",
                               twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "English", twmMarks = []}},
     LeerZeile, LeerZeile,
     TextZeile{ttt = AllCaps0,
               ttx =
                 TextWithMarks{twm =
                                 "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE'S ADVENTURES IN WONDERLAND ***",
                               twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchIgnoreEnd,
                 ttx = TextWithMarks{twm = "", twmMarks = []}},
     LeerZeile, LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "English", twmMarks = []}},
     LeerZeile, LeerZeile, LeerZeile, LeerZeile, LeerZeile,
     MarkupZeile{ttok = BuchTitel,
                 ttx =
                   TextWithMarks{twm = "ALICE'S ADVENTURES IN WONDERLAND",
                                 twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchAuthor,
                 ttx = TextWithMarks{twm = "Lewis Carroll", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchPublikationDetail,
                 ttx =
                   TextWithMarks{twm = "THE MILLENNIUM FULCRUM EDITION 3.0",
                                 twmMarks = []}},
     LeerZeile, LeerZeile,
     MarkupZeile{ttok = BuchHL1,
                 ttx =
                   TextWithMarks{twm = "CHAPTER I. Down the Rabbit-Hole",
                                 twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "Alice was beginning to get very tired of sitting by her sister on the",
                               twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "bank.", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "Deutsch", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "Ein deutscher Satz.", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "English", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "There was nothing so VERY remarkable in that.",
                               twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "In another moment down went Alice after it, never once considering how",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "in the world she was to get out again.",
                               twmMarks = []}},
     LeerZeile, LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = ".gedicht", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "'Fury said to a", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "mouse, That he", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "met in the", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "house.", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchHL1,
                 ttx =
                   TextWithMarks{twm =
                                   "CHAPTER IV. The Rabbit Sends in a Little Bill",
                                 twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "It was the White Rabbit, trotting slowly back again, and looking",
                               twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "anxiously about as it went, as if it had lost something .",
                               twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchIgnore,
                 ttx = TextWithMarks{twm = "THE END", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchIgnoreTo,
                 ttx = TextWithMarks{twm = "", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm =
                                 "End of Project Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll",
                               twmMarks = []}},
     LeerZeile, LeerZeile,
     MarkupZeile{ttok = BuchIgnoreEnd,
                 ttx = TextWithMarks{twm = "", twmMarks = []}},
     LeerZeile,
     MarkupZeile{ttok = BuchEnde,
                 ttx = TextWithMarks{twm = "", twmMarks = []}}]
