test_that("Can run example", {
  res <- example_run("examples_cpp/function-simple.cpp", "examples")
  expect_equal(names(res), c("input", "output"))
  expect_equal(res$input, readLines("examples_cpp/function-simple.cpp"))
  expect_equal(res$output, c("2 + 2 = 4", "1 + 4 = 5"))
})


test_that("can compile simple package", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  path_include <- file.path(tmp, "inst/include")
  path_examples <- file.path(tmp, "inst/cppdoc/examples")

  file.create(file.path(tmp, "NAMESPACE"))
  writeLines(c("Package: cpptest.ex1",
               "Version: 0.0.1",
               "Licence: CC0"),
             file.path(tmp, "DESCRIPTION"))
  dir.create(path_include, FALSE, TRUE)
  file_copy("examples/function-simple.hpp", path_include)
  dir.create(path_examples, FALSE, TRUE)
  file_copy("examples_cpp/function-simple.cpp", path_examples)

  suppressMessages(cppdoc_examples_run(tmp))

  path_rds <- file.path(tmp, "inst/cppdoc/examples/function-simple.rds")
  expect_true(file.exists(path_rds))
  expect_equal(
    readRDS(path_rds),
    example_run("examples_cpp/function-simple.cpp", "examples"))
})
