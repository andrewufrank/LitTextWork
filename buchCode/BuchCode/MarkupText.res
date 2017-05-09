
--the input which could be a file
result0B = unlines'  ["adfka;dfaskl"
            ,"55"
            ,""
            ,"1960 is a good"
            ,"abcpawerqe"
            ,"66"
            , "als\n\f\n[54/0002]\nda\223 man ihm gleichzeitig,"
            , "als\r\n\f\r\n[54/0002]\r\nda\223 man ihm gleichzeitig,"
            ,""
            ,".titel TIT"
            ,"defpageerest"
            ,"77"] ::  Text

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

result4Bx =
  ".sprache German\r\n.isbn ISBN 3-88769-324-8\r\n.author Yoko Tawada\r\n.titel \
  \Das nackte Auge\r\n.verlag konkursbuch verlag claudia gehrke\r\n.dedication f\252r C.D.\r\n6\r\n.hl1 \
  \Kapitel 1 REPULSION\r\nEin gefilmtes Auge, angeheftet an einem bewusstlosen K\246rper. Es sieht nichts, \
  \denn die Kamera hat ihm schon die Sehkraft geraubt.  Er hatte uns mehrmals von seinem Aufenthalt in \
  \Berlin und einem gewissen \"Pergamonmuseum\" erz\228hlt. \"Pergamon\"\r\n7\r\nklang wie der Name \
  \eines Wandervogels, und uns gefiel die Vorstellung des Berliner Himmels, in dem dieser Vogel flatterte.  \
  \Einer\r\n8\r\nvon ihnen nahm mir meine Reisetasche ab. Was sollte man einem solchen Manne schreiben, \
  \der sich offenbar verrannt hatte. Sollte man ihm vielleicht raten, wieder nach Hause zu kommen, \
  \seine Existenz hierher zu verlegen, alle die alten freundschaftlichen Beziehungen wieder \
  \aufzunehmen \8212 wof\252r ja kein Hindernis bestand \8212 und im \252brigen auf die Hilfe der Freunde \
  \zu vertrauen? Das bedeutete aber nichts anderes, als\r\n\f\r\n[54/0002]\r\nda\223 man ihm gleichzeitig, \
  \je schonender, desto kr\228nkender, sagte, da\223 seine bisherigen Versuche mi\223lungen seien, \
  \da\223 er endlich von ihnen ablassen solle, da\223 er zur\252ckkehren und sich als ein f\252r immer \
  \Zur\252ckgekehrter von allen mit gro\223en Augen anstaunen lassen m\252sse, da\223 nur seine Freunde \
  \etwas verst\252nden und da\223 er ein altes Kind sei, das den erfolgreichen, zu Hause gebliebenen \
  \Freunden einfach zu folgen habe.Er schien etwas erschrocken zu sein, wahrscheinlich, weil sie unerwartet \
  \leicht war. \r\n.ende\r\n"

result0BA =
    [KurzeZeile "adfka;dfaskl", ZahlZeile "55",
     TextZeile "1960 is a good", KurzeZeile "abcpawerqe",
     ZahlZeile "66", KurzeZeile "als", NeueSeite, ZahlZeile "[54/0002]",
     TextZeile "da\223 man ihm gleichzeitig,", KurzeZeile "als",
     NeueSeite, ZahlZeile "[54/0002]",
     TextZeile "da\223 man ihm gleichzeitig,", LeerZeile,
     MarkupZeile BuchTitel "TIT", KurzeZeile "defpageerest",
     ZahlZeile "77"]

result1BA =
    [LeerZeile, MarkupZeile BuchSprache "German", LeerZeile,
     MarkupZeile BuchAuthor "PETER WATERHOUSE", LeerZeile,
     MarkupZeile BuchTitel "(Krieg f\252r Welt)", ZahlZeile "11",
     MarkupZeile BuchHL1 "Unsere Br\228uche werden lebendig",
     ZahlZeile "12",
     TextZeile
       "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
     LeerZeile, KurzeZeile "Er fragte sich als zweiter Paragraph.",
     ZahlZeile "13", MarkupZeile BuchEnde ""]
result2BA =
    [LeerZeile, MarkupZeile BuchSprache "German", LeerZeile,
     MarkupZeile BuchAuthor "PETER WATERHOUSE", LeerZeile,
     MarkupZeile BuchTitel
       "(Krieg f\252r Welt) - als test fuer paragraph per zeile",
     ZahlZeile "11",
     MarkupZeile BuchHL1 "Unsere Br\228uche werden lebendig",
     ZahlZeile "12",
     KurzeZeile
       "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
     ZahlZeile "13",
     ParaZeile
       "Es war einmal ein Dorf in einem Tal, in dem kein Reis wuchs. Eine schwangere Frau hatte einen Fisch gefunden und verzehrte ihn, weil sie hungrig war, roh und ohne daran zu denken, ihn mit den anderen im Dorf zu teilen. Sie gebar einen sch\246nen Sohn. Sp\228ter aber wuchsen auf ihrem K\246rper Schuppen und sie wurde zu einem gro\223en Fisch. Weil sie zu Lande nicht mehr wohnen konnte, f\252hrte sie fortan ein einsames Leben im Fluss. Ihr Sohn wurde von einem Alten im Dorf aufgezogen. Zu allen Zeiten verh\246hnen die Knaben die M\252tter der anderen im Streit. Sie streiten, ohne zu verstehen, was sie sagen: \187Deine Mutter ist eine Hure!\171",
     ParaZeile
       "Ich wusch mein Gesicht mit wei\223em Sand. Nur so konnte ich meine Haut, die zu einer W\252ste geworden war, wieder glatt bekommen. Man sagt, dass dieser Sand von den Knochen eines Dinosauriers stamme, von Knochen, die die Wellen des Meeres lange gewaschen haben und die die Sonne getrocknet hat. Ich verteilte ihn auf meine Handfl\228chen und legte diese auf mein Gesicht; sie fingen durch das Fleisch hindurch mit meinen Knochen ein Gespr\228ch an. Ich konnte die Form meines Sch\228dels in meinen H\228nden genau sp\252ren. Au\223er der aus Licht gewordenen Haut und des aus Wasser gewordenen Fleisches gibt es noch einen K\246rper. Aber solange ich lebe, kann niemand diesen K\246rper umarmen.",
     MarkupZeile BuchEnde ""]
result3BA =
    [LeerZeile, MarkupZeile BuchSprache "German", LeerZeile,
     LeerZeile,
     MarkupZeile BuchTitel "test fuer paragraphen mit mehreren zeilen",
     MarkupZeile BuchIgnore
       "Der Autor dankt dem Deutschen Literaturfonds f\252r die Unterst\252tzung.",
     MarkupZeile BuchCopyright
       "\169 2006 Jung und Jung, Salzburg und Wien",
     MarkupZeile BuchIgnore "Satz: Media Design: Riznerat, Salzburg",
     MarkupZeile BuchIgnore "Druck: Friedrich Pustet, Regensburg",
     MarkupZeile BuchISBN "ISBN 3-902497-13-0, 978-3-902497-13-0",
     LeerZeile, LeerZeile, LeerZeile,
     KurzeZeile ",author PETER WATERHOUSE",
     MarkupZeile BuchIgnore "(Krieg und Welt)", LeerZeile,
     MarkupZeile BuchVerlag "JUNG UND JUNG", LeerZeile, LeerZeile,
     LeerZeile, LeerZeile, LeerZeile,
     MarkupZeile BuchHL1 "Unsere Namen werden lebendig",
     KurzeZeile "Was ist ihm fremd und was sein eigen?",
     TextZeile
       "Ich ging aus dem Dorf und tr\228umte vor mich hin einen Satz, oder es",
     TextZeile
       "waren ein paar Worte oder Worte aus Worten kommend, eine halbe",
     TextZeile
       "Melodie. Das Dorf bald tausend Meter zur\252ck, das W\228ldchen eine halbe",
     TextZeile
       "Meile hinter Wiese und Wiese: Das Dorf tanzte heran und war wie mit",
     TextZeile
       "der Hand ber\252hrbar und das W\228ldchen gleichwohl ein Wegbegleiter und",
     TextZeile
       "die vielen Wiesen ein einziges Hemd. Gerettet. Wovor das Dorf gerettet?",
     TextZeile
       "Ein Engel legt mir die H\228nde auf den K\246rper? Wald eine Kinderschar?",
     TextZeile
       "Alle meine Kinder? Mehr als ein Vater, Land aller V\228ter; stehen da auf",
     KurzeZeile "der leeren Wiese die V\228ter?",
     TextZeile
       "Ich stand. Wie standen die? Ich ging wenige Schritte und sagte mir hin:",
     TextZeile
       "Ich werde geschoben. Stand. Ich werde gehoben. Es war still. Ich werde",
     TextZeile
       "geh\246rt. Ich h\246rte. Ich werde geh\246rt. Es war kein Haus da. Es ist kein",
     TextZeile
       "Haus da. Ein Strauch rauschte. Er rauscht, so sagte ich es mir. Es war in",
     TextZeile
       "diesem Aus-dem-Dorf-Gehen kein Vergehen. Gar kein Leichenbegr\228bnis.",
     TextZeile
       "Warum ziehen hier keine Leichenz\252ge? Wo sind die Toten? Wo hingelegt?",
     TextZeile
       "Der Ruf auf der Dorfstra\223e - den ich jetzt h\246rte - wird f\252nfzig und mehr",
     TextZeile
       "Jahre durchrufen. Der Ruf ist ein Mensch, der lang ist. Ich nehme den",
     TextZeile
       "Dorfruf mit wie mich selbst, hab ihn wie ein Auge und Ohr und wie",
     TextZeile
       "Finger so viel. Ich \246ffnete die Hand, hielt sie ge\246ffnet vor mich hin, die",
     ZahlZeile "51",
     TextZeile
       "Innenfl\228che aufw\228rts w\252st und offen. Was w\252rde mit der Hand gesche-",
     TextZeile
       "hen? Ich schaute sie lange an. Um die Hand eine w\252ste, wei\223e Welt.",
     LeerZeile, KurzeZeile "Meine Hand war rot.",
     TextZeile
       "Und wendete mich und schaute. Da war das Dorf. Es war braun und",
     TextZeile
       "rot. Es war etwas, ein Fleck, eine dunkle Frucht. Es rauchte aus Stellen",
     TextZeile
       "der Frucht. Das Dorf schwebte an einer Art von Asth\246he und in einer Art",
     TextZeile
       "und Weise. Tiefer als das Dorf waren Wiesen, das Gr\252ne. Das Dorf jetzt",
     KurzeZeile
       "nicht viel gr\246\223er als meine Hand. Fast ein Auge.",
     LeerZeile, MarkupZeile BuchEnde "", LeerZeile, LeerZeile,
     LeerZeile, LeerZeile, LeerZeile, LeerZeile, LeerZeile, LeerZeile]

result4BA =
    [MarkupZeile BuchSprache "German",
     MarkupZeile BuchISBN "ISBN 3-88769-324-8",
     MarkupZeile BuchAuthor "Yoko Tawada",
     MarkupZeile BuchTitel "Das nackte Auge",
     MarkupZeile BuchVerlag "konkursbuch verlag claudia gehrke",
     MarkupZeile BuchDedikation "f\252r C.D.", ZahlZeile "6",
     MarkupZeile BuchHL1 "Kapitel 1 REPULSION",
     ParaZeile
       "Ein gefilmtes Auge, angeheftet an einem bewusstlosen K\246rper. Es sieht nichts, denn die Kamera hat ihm schon die Sehkraft geraubt.  Er hatte uns mehrmals von seinem Aufenthalt in Berlin und einem gewissen \"Pergamonmuseum\" erz\228hlt. \"Pergamon\"",
     ZahlZeile "7",
     ParaZeile
       "klang wie der Name eines Wandervogels, und uns gefiel die Vorstellung des Berliner Himmels, in dem dieser Vogel flatterte.  Einer",
     ZahlZeile "8",
     ParaZeile
       "von ihnen nahm mir meine Reisetasche ab. Was sollte man einem solchen Manne schreiben, der sich offenbar verrannt hatte. Sollte man ihm vielleicht raten, wieder nach Hause zu kommen, seine Existenz hierher zu verlegen, alle die alten freundschaftlichen Beziehungen wieder aufzunehmen \8212 wof\252r ja kein Hindernis bestand \8212 und im \252brigen auf die Hilfe der Freunde zu vertrauen? Das bedeutete aber nichts anderes, als",
     NeueSeite, ZahlZeile "[54/0002]",
     ParaZeile
       "da\223 man ihm gleichzeitig, je schonender, desto kr\228nkender, sagte, da\223 seine bisherigen Versuche mi\223lungen seien, da\223 er endlich von ihnen ablassen solle, da\223 er zur\252ckkehren und sich als ein f\252r immer Zur\252ckgekehrter von allen mit gro\223en Augen anstaunen lassen m\252sse, da\223 nur seine Freunde etwas verst\252nden und da\223 er ein altes Kind sei, das den erfolgreichen, zu Hause gebliebenen Freunden einfach zu folgen habe.Er schien etwas erschrocken zu sein, wahrscheinlich, weil sie unerwartet leicht war. ",
     MarkupZeile BuchEnde ""]
