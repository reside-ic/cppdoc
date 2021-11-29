test_that("can read simple typedef", {
  path <- doxygen_run_one("examples/typedef-simple.hpp")
  ref <- read_reference("examples/typedef-simple.txt")
  contents <- data.frame(kind = "typedef", name = "ex::real_type")
  x <- extract(path, contents)[[1L]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::real_type")
  expect_equal(x$definition_short, "using ex::real_type = double")
  expect_equal(x$args, "")
  brief <- list(list(
    type = "para",
    value = list(list(type = "text", value = "This type is documented. "))))
  expect_equal(x$brief, brief)
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_typedef(x)), ref)
})


test_that("can read undocumented typedef", {
  path <- doxygen_run_one("examples/typedef-undocumented.hpp")
  ref <- read_reference("examples/typedef-undocumented.txt")
  contents <- data.frame(kind = "typedef", name = "ex::int_type")
  x <- extract(path, contents)[[1L]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::int_type")
  expect_equal(x$definition_short, "using ex::int_type = int")
  expect_equal(x$args, "")
  expect_null(x$brief)
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_typedef(x)), ref)
})


test_that("can read simple function", {
  path <- doxygen_run_one("examples/function-simple.hpp")
  ref <- read_reference("examples/function-simple.txt")
  contents <- data.frame(kind = "function", name = "ex::add")
  x <- extract(path, contents)[[1L]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::add")
  expect_equal(x$value, "double")
  expect_equal(x$param, list(list(type = "double", name = "x"),
                             list(type = "double", name = "y")))
  expect_equal(x$args, "(double x, double y)")
  expect_null(x$brief)

  expect_equal(clean_whitespace(render_function(x)), ref)
})


test_that("can read simple define", {
  path <- doxygen_run_one("examples/define-simple.hpp")
  ref <- read_reference("examples/define-simple.txt")
  contents <- data.frame(kind = "define", name = "CONSTANT")
  x <- extract(path, contents)[[1]]

  expect_equal(x$name, "CONSTANT")
  expect_equal(x$value, "1")
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_define(x)), ref)
})


test_that("can read simple enum", {
  path <- doxygen_run_one("examples/enum-simple.hpp")
  ref <- read_reference("examples/enum-simple.txt")
  contents <- data.frame(kind = "enum", name = "ex::fruit")
  res <- extract(path, contents)

  expect_length(res, 1)  

  x <- res[[1]]
  expect_equal(x$name, "ex::fruit")
  expect_equal(x$strong, "yes")
  expect_equal(vcapply(x$enumvalues, function(x) x$name),
               c("apple", "banana", "cherry"))
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_enum(x)), ref)
})


test_that("can read simple enum", {
  path <- doxygen_run_one("examples/class-simple.hpp")
  ref <- read_reference("examples/class-simple.txt")
  contents <- data.frame(kind = "class", name = "ex::test")
  x <- extract(path, contents)[[1]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::test")

  expect_length(x$sections, 1)
  expect_equal(x$sections[[1]]$kind, "public-func")
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_class(x)), ref)
})


test_that("Can render class fields", {
  path <- doxygen_run_one("examples/class-field.hpp")
  ref <- read_reference("examples/class-field.txt")
  contents <- data.frame(kind = "class", name = "ex::test")
  x <- extract(path, contents)[[1]]

  expect_equal(x$name, "ex::test")

  expect_length(x$sections, 1)
  expect_equal(x$sections[[1]]$kind, "public-attrib")

  expect_equal(clean_whitespace(render_class(x)), ref)
})
