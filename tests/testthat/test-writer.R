test_that("complete conversion test 1", {
    test_file <- system.file("standalone/test.bbl",
                               package = "rebib")
    exp_test_file <- system.file("standalone/expected-test.bib",
                             package = "rebib")
    dir.create(temp_test_folder <- file.path(tempdir(), "testdir"))
    file.copy(test_file, temp_test_folder)
    temp_test_file_path <- xfun::normalize_path(paste(temp_test_folder,"test.bbl",sep="/"))
    rebib::biblio_converter(temp_test_file_path)
    test_bib_file <- xfun::with_ext(temp_test_file_path,"bib")
    expect_equal(readLines(test_bib_file), readLines(exp_test_file))
    unlink(temp_test_folder, recursive = TRUE)
    dir.exists(temp_test_folder)
})
