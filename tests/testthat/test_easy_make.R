## test_that("Makefile Builds", {
##     testthat::skip_on_cran()
##     expected <- readLines("expected_Makefile")

##     easy_make(detect_dependencies(path = "../../inst/test_project"))
##     actual <- readLines("Makefile")

##     expect_equal(expected, actual)
##     )
## })
