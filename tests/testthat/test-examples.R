test_that("can read simple typedef", {
  path <- doxygen_run_one("examples/simple-typedef.hpp")
  ref <- read_reference("examples/simple-typedef.txt")
  contents <- data.frame(kind = "typedef",
                         name = c("ex::real_type", "ex::int_type"))
  res <- extract(path, contents)

  expect_length(res, 2)

  real_type <- res[[1]]
  expect_null(real_type$tparam)
  expect_equal(real_type$name, "ex::real_type")
  expect_equal(real_type$definition_short, "using ex::real_type = double")
  expect_equal(real_type$args, "")
  brief <- list(list(
    type = "para",
    value = list(list(type = "text", value = "This type is documented. "))))
  expect_equal(real_type$brief, brief)
  expect_null(real_type$detail)

  int <- res[[2]]
  expect_null(int$tparam)
  expect_equal(int$name, "ex::int_type")
  expect_equal(int$definition_short, "using ex::int_type = int")
  expect_equal(int$args, "")
  expect_null(int$brief)
  expect_null(int$detail)

  expect_equal(
    clean_whitespace(render_typedef(real_type)),
    ref[["ex::real_type"]])
  expect_equal(
    clean_whitespace(render_typedef(int)),
    ref[["ex::int"]])
})


test_that("can read simple function", {
  path <- doxygen_run_one("examples/simple-function.hpp")
  ref <- read_reference("examples/simple-function.txt")
  contents <- data.frame(kind = "function", name = "ex::add")
  res <- extract(path, contents)

  expect_length(res, 1)  

  x <- res[[1]]
  expect_null(x$tparam)
  expect_equal(x$name, "ex::add")
  expect_equal(x$value, "double")
  expect_equal(x$param, list(list(type = "double", name = "x"),
                             list(type = "double", name = "y")))
  expect_equal(x$args, "(double x, double y)")
  expect_null(x$brief)

  expect_equal(
    clean_whitespace(render_function(x)),
    ref[["ex::add"]])
})


test_that("can read simple define", {
  path <- doxygen_run_one("examples/simple-define.hpp")
  ref <- read_reference("examples/simple-define.txt")
  contents <- data.frame(kind = "define", name = "CONSTANT")
  res <- extract(path, contents)

  expect_length(res, 1)  

  x <- res[[1]]
  expect_equal(x$name, "CONSTANT")
  expect_equal(x$value, "1")
  expect_null(x$detail)

  expect_equal(
    clean_whitespace(render_define(x)),
    ref[["CONSTANT"]])
})


test_that("can read simple enum", {
  path <- doxygen_run_one("examples/simple-enum.hpp")
  ref <- read_reference("examples/simple-enum.txt")
  contents <- data.frame(kind = "enum", name = "ex::fruit")
  res <- extract(path, contents)

  expect_length(res, 1)  

  x <- res[[1]]
  expect_equal(x$name, "ex::fruit")
  expect_equal(x$strong, "yes")
  expect_equal(vcapply(x$enumvalues, function(x) x$name),
               c("apple", "banana", "cherry"))
  expect_null(x$detail)

  expect_equal(
    clean_whitespace(render_enum(x)),
    ref[["ex::fruit"]])
})


test_that("can read simple enum", {
  path <- doxygen_run_one("examples/simple-class.hpp")
  ref <- read_reference("examples/simple-class.txt")
  contents <- data.frame(kind = "class", name = "ex::test")
  res <- extract(path, contents)

  expect_length(res, 1)

  x <- res[[1]]
  expect_null(x$tparam)
  expect_equal(x$name, "ex::test")

  expect_length(x$sections, 1)
  expect_equal(x$sections[[1]]$kind, "public-func")
  expect_null(x$detail)

  expect_equal(
    clean_whitespace(render_class(x)),
    ref[["ex::test"]])
})


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
