# A unique case where the second newblock is missing
test_1 <- c(
    "\\bibitem[Montes(1996)]{montes96}",
    "M.J.~Montes.",
    "\\newblock Butcher's Algorithm for Calculating the Date of Easter in",
    "the Gregorian Calendar, 1996."
)
test_that("parser parsing bibliography test 1", {
    out <- rebib:::bibliography_parser(test_1)
    expect_equal(out$author, "M.J.~Montes")
    expect_equal(out$journal, "the Gregorian Calendar")
    expect_equal(out$year, "1996")
    expect_equal(out$title, "Butcher's Algorithm for Calculating the Date of Easter in")
    expect_equal(out$URL, NULL)
    expect_equal(out$unique_id, "montes96")
    expect_equal(out$pages, NULL)

})
# This test case has 3 line long title which will challenge the parser
test_2 <- c(
    "\\bibitem[Tremblay(2012)]{Tremblay:2012}",
    "A.~Tremblay.",
    "\\newblock \\emph{LMERConvenienceFunctions: A suite of functions to back-fit",
    "  fixed effects and forward-fit random effects, as well as other miscellaneous",
    "  functions.}, 2012.",
    "\\newblock URL \\url{http://CRAN.R-project.org/package=LMERConvenienceFunctions}.",
    "\\newblock R package version 1.6.8.2."
)

test_that("parser parsing bibliography test 2", {
    out <- rebib:::bibliography_parser(test_2)
    expect_equal(out$author, "A.~Tremblay")
    expect_equal(out$journal, "R package version 1.6.8.2")
    expect_equal(out$year, "2012")
    expect_equal(out$title, "LMERConvenienceFunctions: A suite of functions to back-fit   fixed effects and forward-fit random effects, as well as other miscellaneous   functions., ")
    expect_equal(out$URL, "http://CRAN.R-project.org/package=LMERConvenienceFunctions")
    expect_equal(out$unique_id, "Tremblay:2012")
    expect_equal(out$pages, NULL)
})

# This example has a slightly complicated journal field
test_3 <- c(
    "\\bibitem[Akaike(1973)]{Akaike:1973}",
    "H.~Akaike.",
    "\\newblock Information theory and an extension of the maximum likelihood",
    "principle.",
    "\\newblock In {Petrov BN} and {Csaki BF}, editors, \\emph{Second international",
    "   symposium on information theory}, pages 267--281. Academiai Kiado, Budapest,",
    "1973."
)

test_that("parser parsing bibliography test 3", {
    out <- rebib:::bibliography_parser(test_3)
    expect_equal(out$author, "H.~Akaike")
    expect_equal(out$journal, "In Petrov BN and Csaki BF editors Second international   symposium on information theory pages  Academiai Kiado Budapest")
    expect_equal(out$year, "1973")
    expect_equal(out$title, "Information theory and an extension of the maximum likelihood principle")
    expect_equal(out$URL, NULL)
    expect_equal(out$unique_id, "Akaike:1973")
    expect_equal(out$pages, "267--281")
})
# This test case has a long author name field.
test_4 <- c(
    "\\bibitem[Limas et~al.(2007)Limas, Joaquín B. Ordieres~Meré, de~Pisón~Ascacibar,",
    "         Espinoza, and Elías]{AMORE}",
    "M.~C. Limas, E.~P. V.~G. Joaquín B. Ordieres~Meré, F.~J.~M. de~Pisón~Ascacibar,",
    "A.~V.~P. Espinoza, and F.~A. Elías.",
    "\\newblock \\emph{AMORE: A MORE Flexible Neural Network Package}, 2007.",
    "\\newblock URL",
    "\\url{http://wiki.r-project.org/rwiki/doku.php?id=packages:cran:amore}.",
    "\\newblock R package version 0.2-11."
)

test_that("parser parsing bibliography test 4", {
    out <- rebib:::bibliography_parser(test_4)
    expect_equal(out$author, "M.~C. Limas, E.~P. V.~G. Joaquín B. Ordieres~Meré, F.~J.~M. de~Pisón~Ascacibar, A.~V.~P. Espinoza, and F.~A. Elías")
    expect_equal(out$journal, "R package version 0.2-11")
    expect_equal(out$year, "2007")
    expect_equal(out$title, "AMORE: A MORE Flexible Neural Network Package, ")
    expect_equal(out$URL, "http://wiki.r-project.org/rwiki/doku.php?id=packages:cran:amore")
    expect_equal(out$unique_id, "AMORE")
    expect_equal(out$pages, NULL)
})
# the publisher/journal field will be missing here
test_5 <- c(
    "\\bibitem[T{\\o}ndering(2008)]{tondering08}",
    "C.~T{\\o}ndering.",
    "\\newblock Frequently Asked Questions about Calendars, 2008.",
    "\\newblock URL \\url{http://www.tondering.dk/claus/calendar.html}."
    )

test_that("parser parsing bibliography test 5", {
    out <- rebib:::bibliography_parser(test_5)
    expect_equal(out$author, "C.~T{\\o}ndering")
    expect_equal(out$journal, NULL)
    expect_equal(out$year, "2008")
    expect_equal(out$title, "Frequently Asked Questions about Calendars, ")
    expect_equal(out$URL, "http://www.tondering.dk/claus/calendar.html")
    expect_equal(out$unique_id, "tondering08")
    expect_equal(out$pages, NULL)
})

test_6 <- c(
    "\\bibitem[Wuertz et~al.(2013)Wuertz, with contribution~from Michal~Miklovic,",
    "Boudt, Chausse, and {others}]{WuertzChalabiMiklovicBoudtChausseOthers2013}",
    "D.~Wuertz, Y.~C. with contribution~from Michal~Miklovic, C.~Boudt, P.~Chausse,",
    "and {others}.",
    "\\newblock \\emph{{fGarch}: Rmetrics -- Autoregressive Conditional",
    "Heteroskedastic Modelling}, 2013.",
    "\\newblock URL \\url{http://CRAN.R-project.org/package=fGarch}.",
    "\\newblock R package version 3010.82."
)

test_that("parser parsing bibliography test 6", {
    out <- rebib:::bibliography_parser(test_6)
    expect_equal(out$author, "D.~Wuertz, Y.~C. with contribution~from Michal~Miklovic, C.~Boudt, P.~Chausse, and {others}")
    expect_equal(out$journal, "R package version 3010.82")
    expect_equal(out$year, "2013")
    expect_equal(out$title, "fGarch: Rmetrics -- Autoregressive Conditional Heteroskedastic Modelling, ")
    expect_equal(out$URL, "http://CRAN.R-project.org/package=fGarch")
    expect_equal(out$unique_id, "WuertzChalabiMiklovicBoudtChausseOthers2013")
    expect_equal(out$pages, NULL)
})

test_7 <- c("\\bibitem[Hampton et~al.(2006)Hampton, Scheuerell, and",
            "Schindler]{Hamptonetal2006}",
            "S.~E. Hampton, M.~D. Scheuerell, and D.~E. Schindler.",
            "\\newblock Coalescence in the {L}ake {W}ashington story: interaction strengths",
            "in a planktonic food web.",
            "\\newblock \\emph{Limnology and Oceanography}, 51\\penalty0 (5):\\penalty0",
            "2042--2051, 2006."
            )
test_that("parser parsing bibliography test 7", {
    out <- rebib:::bibliography_parser(test_7)
    expect_equal(out$author, "S.~E. Hampton, M.~D. Scheuerell, and D.~E. Schindler")
    expect_equal(out$journal, "Limnology and Oceanography 51 (5):")
    expect_equal(out$year, "2006")
    expect_equal(out$title, "Coalescence in the Lake Washington story: interaction strengths in a planktonic food web")
    expect_equal(out$URL, NULL)
    expect_equal(out$unique_id, "Hamptonetal2006")
    expect_equal(out$pages, "2042--2051")
})

test_8 <- c("\\bibitem[Markussen(2013)]{pcomm:SM13}",
            "S.~Markussen.",
            "\\newblock Personal Communication, 2013."
            )
test_that("parser parsing bibliography test 8", {
    out <- rebib:::bibliography_parser(test_8)
    expect_equal(out$author, "S.~Markussen")
    expect_equal(out$journal, "Personal Communication")
    expect_equal(out$year, "2013")
    expect_equal(out$title, NULL)
    expect_equal(out$URL, NULL)
    expect_equal(out$unique_id, "pcomm:SM13")
})

test_9 <- c("\\bibitem[Dem{\\v{s}}ar et~al.(2013)Dem{\\v{s}}ar, Curk, Erjavec, Gorup,",
            "Ho{\\v{c}}evar, Milutinovi{\\v{c}}, Mo{\\v{z}}ina, Polajnar, Toplak,",
            "Stari{\\v{c}}, et~al.]{demvsar2013orange}",
            "J.~Dem{\\v{s}}ar, T.~Curk, A.~Erjavec, {\\v{C}}.~Gorup, T.~Ho{\\v{c}}evar,",
            "M.~Milutinovi{\\v{c}}, M.~Mo{\\v{z}}ina, M.~Polajnar, M.~Toplak,",
            "A.~Stari{\\v{c}}, et~al.",
            "\\newblock Orange: data mining toolbox in python.",
            "\\newblock \\emph{The Journal of Machine Learning Research}, 14\\penalty0",
            "(1):\\penalty0 2349--2353, 2013.")

test_that("parser parsing bibliography test 9", {
    out <- rebib:::bibliography_parser(test_9)
    expect_equal(out$author, "J.~Dem{\\v{s}}ar, T.~Curk, A.~Erjavec, {\\v{C}}.~Gorup, T.~Ho{\\v{c}}evar, M.~Milutinovi{\\v{c}}, M.~Mo{\\v{z}}ina, M.~Polajnar, M.~Toplak, A.~Stari{\\v{c}}, et~al")
    expect_equal(out$journal, "The Journal of Machine Learning Research 14(1):")
    expect_equal(out$year, "2013")
    expect_equal(out$title, "Orange: data mining toolbox in python")
    expect_equal(out$URL, NULL)
    expect_equal(out$pages, "2349--2353")
    expect_equal(out$unique_id, "demvsar2013orange")
})
