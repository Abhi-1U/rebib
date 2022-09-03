test_that("complete conversion test 1", {
    test_file <- system.file("standalone/test.bbl",
                               package = "rebib")
    exp_test_file <- system.file("standalone/expected-test.bib",
                             package = "rebib")
    rebib::biblio_convertor(test_file)
    test_bib_file <- gsub(".bbl",".bib",test_file)
    expect_equal(readLines(test_bib_file), readLines(exp_test_file))
})
