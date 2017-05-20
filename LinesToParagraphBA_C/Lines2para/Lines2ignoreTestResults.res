

result1C  :: [TZ]
result1C  =
    []

result2C  :: [TZ]
result2C  =
    []


result3C  =
    []

result4C =

    []





-- result from first step in lines2para (ignore, language)

result1BAD =


    [TZleer{tzloc = TextLoc{tlpage = "11", tlline = 1}},
     TZleer{tzloc = TextLoc{tlpage = "11", tlline = 3}},
     TZmarkup{tzloc = TextLoc{tlpage = "11", tlline = 4},
              tztext = TextWithMarks{twm = "PETER WATERHOUSE", twmMarks = []},
              tztok = BuchAuthor, tzlang = German},
     TZleer{tzloc = TextLoc{tlpage = "11", tlline = 5}},
     TZmarkup{tzloc = TextLoc{tlpage = "11", tlline = 6},
              tztext = TextWithMarks{twm = "(Krieg f\252r Welt)", twmMarks = []},
              tztok = BuchTitel, tzlang = German},
     TZmarkup{tzloc = TextLoc{tlpage = "12", tlline = 8},
              tztext =
                TextWithMarks{twm = "Unsere Br\228uche werden lebendig",
                              twmMarks = []},
              tztok = BuchHL1, tzlang = German},
     TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "13", tlline = 10},
            tztext =
              TextWithMarks{twm =
                              "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
                            twmMarks = []},
            tzlang = German},
     TZleer{tzloc = TextLoc{tlpage = "13", tlline = 11}},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "13", tlline = 12},
            tztext =
              TextWithMarks{twm = "Er fragte sich als zweiter Paragraph.",
                            twmMarks = []},
            tzlang = German},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 14}}]



result2BAD =

    [TZleer{tzloc = TextLoc{tlpage = "11", tlline = 1}},
     TZleer{tzloc = TextLoc{tlpage = "11", tlline = 3}},
     TZmarkup{tzloc = TextLoc{tlpage = "11", tlline = 4},
              tztext = TextWithMarks{twm = "PETER WATERHOUSE", twmMarks = []},
              tztok = BuchAuthor, tzlang = German},
     TZleer{tzloc = TextLoc{tlpage = "11", tlline = 5}},
     TZmarkup{tzloc = TextLoc{tlpage = "11", tlline = 6},
              tztext =
                TextWithMarks{twm =
                                "(Krieg f\252r Welt) - als test fuer paragraph per zeile",
                              twmMarks = []},
              tztok = BuchTitel, tzlang = German},
     TZmarkup{tzloc = TextLoc{tlpage = "12", tlline = 8},
              tztext =
                TextWithMarks{twm = "Unsere Br\228uche werden lebendig",
                              twmMarks = []},
              tztok = BuchHL1, tzlang = German},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "13", tlline = 10},
            tztext =
              TextWithMarks{twm =
                              "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
                            twmMarks = []},
            tzlang = German},
     TZtext{tzt = Para0, tzloc = TextLoc{tlpage = "", tlline = 12},
            tztext =
              TextWithMarks{twm =
                              "Es war einmal ein Dorf in einem Tal, in dem kein Reis wuchs. Eine schwangere Frau hatte einen Fisch gefunden und verzehrte ihn, weil sie hungrig war, roh und ohne daran zu denken, ihn mit den anderen im Dorf zu teilen. Sie gebar einen sch\246nen Sohn. Sp\228ter aber wuchsen auf ihrem K\246rper Schuppen und sie wurde zu einem gro\223en Fisch. Weil sie zu Lande nicht mehr wohnen konnte, f\252hrte sie fortan ein einsames Leben im Fluss. Ihr Sohn wurde von einem Alten im Dorf aufgezogen. Zu allen Zeiten verh\246hnen die Knaben die M\252tter der anderen im Streit. Sie streiten, ohne zu verstehen, was sie sagen: \187Deine Mutter ist eine Hure!\171",
                            twmMarks = []},
            tzlang = German},
     TZtext{tzt = Para0, tzloc = TextLoc{tlpage = "", tlline = 13},
            tztext =
              TextWithMarks{twm =
                              "Ich wusch mein Gesicht mit wei\223em Sand. Nur so konnte ich meine Haut, die zu einer W\252ste geworden war, wieder glatt bekommen. Man sagt, dass dieser Sand von den Knochen eines Dinosauriers stamme, von Knochen, die die Wellen des Meeres lange gewaschen haben und die die Sonne getrocknet hat. Ich verteilte ihn auf meine Handfl\228chen und legte diese auf mein Gesicht; sie fingen durch das Fleisch hindurch mit meinen Knochen ein Gespr\228ch an. Ich konnte die Form meines Sch\228dels in meinen H\228nden genau sp\252ren. Au\223er der aus Licht gewordenen Haut und des aus Wasser gewordenen Fleisches gibt es noch einen K\246rper. Aber solange ich lebe, kann niemand diesen K\246rper umarmen.",
                            twmMarks = []},
            tzlang = German},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 14}}]

result6BAD =
    [TZignore{tzloc = TextLoc{tlpage = "", tlline = 2},
              tztext =
                TextWithMarks{twm =
                                "Project Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll",
                              twmMarks = []}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 3}},
     TZignore{tzloc = TextLoc{tlpage = "", tlline = 4},
              tztext =
                TextWithMarks{twm = "test for ignore etc.", twmMarks = []}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 5}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 6}},
     TZignore{tzloc = TextLoc{tlpage = "", tlline = 7},
              tztext =
                TextWithMarks{twm = "Posting Date: June 25, 2008 [EBook #11]",
                              twmMarks = []}},
     TZignore{tzloc = TextLoc{tlpage = "", tlline = 8},
              tztext =
                TextWithMarks{twm = "Release Date: March, 1994", twmMarks = []}},
     TZignore{tzloc = TextLoc{tlpage = "", tlline = 9},
              tztext =
                TextWithMarks{twm = "[Last updated: December 20, 2011]",
                              twmMarks = []}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 10}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 12}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 13}},
     TZignore{tzloc = TextLoc{tlpage = "", tlline = 14},
              tztext =
                TextWithMarks{twm =
                                "*** START OF THIS PROJECT GUTENBERG EBOOK ALICE'S ADVENTURES IN WONDERLAND ***",
                              twmMarks = []}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 15}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 17}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 18}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 20}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 21}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 22}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 23}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 24}},
     TZmarkup{tzloc = TextLoc{tlpage = "", tlline = 25},
              tztext =
                TextWithMarks{twm = "ALICE'S ADVENTURES IN WONDERLAND",
                              twmMarks = []},
              tztok = BuchTitel, tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 26}},
     TZmarkup{tzloc = TextLoc{tlpage = "", tlline = 27},
              tztext = TextWithMarks{twm = "Lewis Carroll", twmMarks = []},
              tztok = BuchAuthor, tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 28}},
     TZmarkup{tzloc = TextLoc{tlpage = "", tlline = 29},
              tztext =
                TextWithMarks{twm = "THE MILLENNIUM FULCRUM EDITION 3.0",
                              twmMarks = []},
              tztok = BuchPublikationDetail, tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 30}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 31}},
     TZmarkup{tzloc = TextLoc{tlpage = "", tlline = 32},
              tztext =
                TextWithMarks{twm = "CHAPTER I. Down the Rabbit-Hole",
                              twmMarks = []},
              tztok = BuchHL1, tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 33}},
     TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 34},
            tztext =
              TextWithMarks{twm =
                              "Alice was beginning to get very tired of sitting by her sister on the",
                            twmMarks = []},
            tzlang = English},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 35},
            tztext = TextWithMarks{twm = "bank.", twmMarks = []},
            tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 36}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 38}},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 39},
            tztext = TextWithMarks{twm = "Ein deutscher Satz.", twmMarks = []},
            tzlang = German},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 40}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 42}},
     TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 43},
            tztext =
              TextWithMarks{twm =
                              "There was nothing so VERY remarkable in that.",
                            twmMarks = []},
            tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 44}},
     TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 45},
            tztext =
              TextWithMarks{twm =
                              "In another moment down went Alice after it, never once considering how",
                            twmMarks = []},
            tzlang = English},
     TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 46},
            tztext =
              TextWithMarks{twm = "in the world she was to get out again.",
                            twmMarks = []},
            tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 47}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 48}},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 49},
            tztext = TextWithMarks{twm = ".gedicht", twmMarks = []},
            tzlang = English},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 50},
            tztext = TextWithMarks{twm = "'Fury said to a", twmMarks = []},
            tzlang = English},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 51},
            tztext = TextWithMarks{twm = "mouse, That he", twmMarks = []},
            tzlang = English},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 52},
            tztext = TextWithMarks{twm = "met in the", twmMarks = []},
            tzlang = English},
     TZtext{tzt = Kurz0, tzloc = TextLoc{tlpage = "", tlline = 53},
            tztext = TextWithMarks{twm = "house.", twmMarks = []},
            tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 54}},
     TZmarkup{tzloc = TextLoc{tlpage = "", tlline = 55},
              tztext =
                TextWithMarks{twm =
                                "CHAPTER IV. The Rabbit Sends in a Little Bill",
                              twmMarks = []},
              tztok = BuchHL1, tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 56}},
     TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 57},
            tztext =
              TextWithMarks{twm =
                              "It was the White Rabbit, trotting slowly back again, and looking",
                            twmMarks = []},
            tzlang = English},
     TZtext{tzt = Text0, tzloc = TextLoc{tlpage = "", tlline = 58},
            tztext =
              TextWithMarks{twm =
                              "anxiously about as it went, as if it had lost something .",
                            twmMarks = []},
            tzlang = English},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 59}},
     TZignore{tzloc = TextLoc{tlpage = "", tlline = 60},
              tztext = TextWithMarks{twm = "THE END", twmMarks = []}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 61}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 63}},
     TZignore{tzloc = TextLoc{tlpage = "", tlline = 64},
              tztext =
                TextWithMarks{twm =
                                "End of Project Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll",
                              twmMarks = []}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 65}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 66}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 68}},
     TZleer{tzloc = TextLoc{tlpage = "", tlline = 69}}]

