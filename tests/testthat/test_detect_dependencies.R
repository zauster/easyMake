test_that("dependencies are detected", {

    load("expected_dependencies.RData")
    expected_dependencies <- as.data.table(expected_dependencies)

    expect_equal(
        detect_dependencies(
            system.file("test_project", package = "easyMake")),
        expected_dependencies)
})
