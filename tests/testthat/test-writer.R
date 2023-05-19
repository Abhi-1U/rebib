test_that("complete conversion test 1", {
    test_file <- system.file("standalone/test.bbl",
                               package = "rebib")
    exp_test_file <- system.file("standalone/expected-test.bib",
                             package = "rebib")
    rebib::biblio_converter(test_file)
    test_bib_file <- xfun::with_ext(test_file,"bib")
    expect_equal(readLines(test_bib_file), readLines(exp_test_file))
})