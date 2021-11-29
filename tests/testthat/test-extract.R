test_that("overloaded functions", {
  path <- doxygen_run_one("examples/function-overload.hpp")
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
      vcapply(el$param, "[[", "type"))
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
