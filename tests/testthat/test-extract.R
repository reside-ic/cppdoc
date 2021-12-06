test_that("overloaded functions", {
  path <- doxygen_run_one("examples_hpp/function-overload.hpp")
  doxygen <- doxygen_contents$new(path)
  expect_error(
    extract_function(doxygen, "ex::f", NULL),
    "Can't decide which 'ex::f' overload you want")
  expect_error(
    extract_function(doxygen, "ex::f", "int"),
    "Did not find function overload 'ex::f(int)'",
    fixed = TRUE)

  types <- function(x) {
    lapply(x, function(el)
      vcapply(el$param, function(x) x$type$value))
  }

  expect_equal(
    types(extract_function(doxygen, "ex::f", list("double"))),
    list("double"))
  expect_equal(
    types(extract_function(doxygen, "ex::f", list(c("double", "double")))),
    list(c("double", "double")))
  expect_equal(
    types(extract_function(doxygen, "ex::f", list("const double *"))),
    list("const double *"))
  expect_equal(
    extract_function(doxygen, "ex::f", "const double *"),
    extract_function(doxygen, "ex::f", "const   double*"))
  expect_equal(
    types(extract_function(doxygen, "ex::f", list(character(0)))),
    list(character(0)))
})


test_that("error if object not found", {
  path <- doxygen_run_one("examples_hpp/typedef-simple.hpp")
  doxygen <- doxygen_contents$new(path)
  expect_error(
    extract_typedef(doxygen, "ex::unknown"),
    "Did not find typedef 'ex::unknown'")
  ## This one can't be triggered easily, but I am certain we'll see it
  ## eventually:
  expect_error(
    check_index(c(TRUE, FALSE, TRUE, FALSE), "typedef", "ex::mytype"),
    "Unexpected ambiguous match for typedef 'ex::mytype'")
})
