test_that("can build simple index, and find things in it", {
  cmp <- function(path, ...) {
    path <- doxygen_run_one(path)
    on.exit(unlink(path, recursive = TRUE))
    doxygen <- doxygen_contents$new(path)
    extract_function(doxygen, ...)[[1L]]
  }

  contents <- data.frame(kind = "function",
                         name = c("ex::add", "ex::f", "ex::f"),
                         args = I(list(NULL, "double", c("double", "double"))),
                         stringsAsFactors = FALSE)
  index <- index_build("examples", "cpptest", contents, TRUE, TRUE)
  expect_s3_class(index, "cppdoc_index")
  expect_equal(nrow(index), 3)

  expect_equal(
    index_find(index, "function", "ex::add", NULL),
    cmp("examples/function-simple.hpp", "ex::add", NULL))
  expect_equal(
    index_find(index, "function", "ex::f", "double"),
    cmp("examples/function-overload.hpp", "ex::f", "double"))

  expect_error(
    index_find(index, "function", "ex::f", NULL),
    "Can't decide which 'ex::f' overload you want; args needed to disambiguate")
  expect_error(
    index_find(index, "function", "ex::f", "int"),
    "Did not find function overload 'ex::f(int)'",
    fixed = TRUE)
  expect_error(
    index_find(index, "function", "ex::unknown_function", NULL),
    "Lookup failure did not find function 'ex::unknown_function'")
})


test_that("Can locate calls in a knitr document", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  file.copy("examples/simple.Rmd", tmp)

  contents <- index_search_rmd(tmp, TRUE)
  expect_equal(nrow(contents), 1L)
  expect_equal(contents$kind, "function")
  expect_equal(contents$name, "ex::add")
  expect_equal(contents$args, I(list(NULL)))
})


test_that("validate cppdoc calls", {
  expect_equal(
    parse_rmd_cppdoc_entry(quote(cppdoc_function("a"))),
    list(kind = "function", name = "a"))
  expect_equal(
    parse_rmd_cppdoc_entry(quote(cppdoc_function("a", "double"))),
    list(kind = "function", name = "a", args = "double"))
  expect_equal(
    parse_rmd_cppdoc_entry(quote(cppdoc_function("a", c("double", "int")))),
    list(kind = "function", name = "a", args = c("double", "int")))
  expect_error(
    parse_rmd_cppdoc_entry(quote(cppdoc_function(a))),
    "'name' must be character in call to 'cppdoc_function'")
  expect_error(
    parse_rmd_cppdoc_entry(quote(plot(1:10))),
    "Invalid function 'plot' within cppdoc block")
  expect_error(
    parse_rmd_cppdoc_entry(quote(TRUE)),
    "Invalid statement 'TRUE' within cppdoc block")
})
