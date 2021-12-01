test_that("can read simple typedef", {
  path <- doxygen_run_one("examples_hpp/typedef-simple.hpp")
  ref <- read_reference("examples_hpp/typedef-simple.txt")
  contents <- data.frame(kind = "typedef", name = "ex::real_type")
  x <- extract(path, NULL, contents)[[1L]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::real_type")
  expect_equal(x$args, "")
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_typedef(x, NULL)), ref)
})


test_that("can read undocumented typedef", {
  path <- doxygen_run_one("examples_hpp/typedef-undocumented.hpp")
  ref <- read_reference("examples_hpp/typedef-undocumented.txt")
  contents <- data.frame(kind = "typedef", name = "ex::int_type")
  x <- extract(path, NULL, contents)[[1L]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::int_type")
  expect_equal(x$args, "")
  expect_null(x$brief)
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_typedef(x, NULL)), ref)
})


test_that("can read simple function", {
  path <- doxygen_run_one("examples_hpp/function-simple.hpp")
  ref <- read_reference("examples_hpp/function-simple.txt")
  contents <- data.frame(kind = "function", name = "ex::add")
  x <- extract(path, NULL, contents)[[1L]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::add")
  expect_equal(x$value, list(type = "text", value = "double"))
  expect_equal(
    x$param,
    list(list(type = list(type = "text", value = "double"), name = "x"),
         list(type = list(type = "text", value = "double"), name = "y")))
  expect_null(x$brief)

  expect_equal(clean_whitespace(render_function(x, NULL)), ref)
})


test_that("Can render a templated function", {
  skip("FIXME")
  path <- doxygen_run_one("examples_hpp/function-templated.hpp")
  ref <- read_reference("examples_hpp/function-templated.txt")
  contents <- data.frame(kind = "function", name = "ex::generic")
  x <- extract(path, NULL, contents)[[1]]

  expect_equal(x$tparam, list("typename T", "typename U"))
  expect_equal(x$name, "ex::generic")
  expect_equal(x$value, "T")
  expect_equal(
    x$param,
    list(list(type = list(type = "text", value = "const T &"), name = "x"),
         list(type = list(type = "text", value = "U"), name = "y")))
  expect_null(x$brief)

  expect_equal(clean_whitespace(render_function(x, NULL)), ref)
})


test_that("can read simple define", {
  path <- doxygen_run_one("examples_hpp/define-simple.hpp")
  ref <- read_reference("examples_hpp/define-simple.txt")
  contents <- data.frame(kind = "define", name = "CONSTANT")
  x <- extract(path, NULL, contents)[[1]]

  expect_equal(x$name, "CONSTANT")
  expect_equal(x$value, list(type = "text", value = "1"))
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_define(x, NULL)), ref)
})


test_that("can read simple enum", {
  path <- doxygen_run_one("examples_hpp/enum-simple.hpp")
  ref <- read_reference("examples_hpp/enum-simple.txt")
  contents <- data.frame(kind = "enum", name = "ex::fruit")
  res <- extract(path, NULL, contents)

  expect_length(res, 1)

  x <- res[[1]]
  expect_equal(x$name, "ex::fruit")
  expect_equal(x$strong, "yes")
  expect_equal(vcapply(x$enumvalues, function(x) x$name),
               c("apple", "banana", "cherry"))
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_enum(x, NULL)), ref)
})


test_that("can read simple class", {
  path <- doxygen_run_one("examples_hpp/class-simple.hpp")
  ref <- read_reference("examples_hpp/class-simple.txt")
  contents <- data.frame(kind = "class", name = "ex::test")
  x <- extract(path, NULL, contents)[[1]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::test")

  expect_length(x$sections, 1)
  expect_equal(x$sections[[1]]$kind, "public-func")
  expect_null(x$detail)

  expect_equal(clean_whitespace(render_class(x, NULL)), ref)
})


test_that("Can render class fields", {
  path <- doxygen_run_one("examples_hpp/class-field.hpp")
  ref <- read_reference("examples_hpp/class-field.txt")
  contents <- data.frame(kind = "class", name = "ex::has_field")
  x <- extract(path, NULL, contents)[[1]]

  expect_equal(x$name, "ex::has_field")

  expect_length(x$sections, 1)
  expect_equal(x$sections[[1]]$kind, "public-attrib")

  expect_equal(clean_whitespace(render_class(x, NULL)), ref)
})


test_that("Can render class typedefs", {
  path <- doxygen_run_one("examples_hpp/class-typedef.hpp")
  ref <- read_reference("examples_hpp/class-typedef.txt")
  contents <- data.frame(kind = "class", name = "ex::has_typedef")
  x <- extract(path, NULL, contents)[[1]]

  expect_equal(x$name, "ex::has_typedef")

  expect_length(x$sections, 1)
  expect_equal(x$sections[[1]]$kind, "public-type")
  expect_equal(x$sections[[1]]$value[[1]]$name, "real_type")
  expect_equal(x$sections[[1]]$value[[2]]$name, "int_type")

  expect_equal(clean_whitespace(render_class(x, NULL)), ref)
})


test_that("linking", {
  path <- doxygen_run_one("examples_hpp/class-link.hpp")
  contents <- data.frame(kind = "class", name = "ex::has_link")
  control <- test_link_control(path)

  ref_link <- read_reference("examples_hpp/class-link-link.txt")
  ref_nolink <- read_reference("examples_hpp/class-link-nolink.txt")

  x <- extract(path, NULL, contents)[[1]]

  expect_equal(clean_whitespace(render_class(x, control)),
               ref_link)
  expect_equal(clean_whitespace(render_class(x, NULL)),
               ref_nolink)
})


test_that("function attributes", {
  path <- doxygen_run_one("examples_hpp/function-attributes.hpp")
  ref <- read_reference("examples_hpp/function-attributes.txt")
  contents <- data.frame(kind = "function", name = "ex::add_gpu")
  x <- extract(path, NULL, contents)[[1]]
  render_function(x, NULL)

  expect_equal(x$name, "ex::add_gpu")

  ## We need to rewrite links here, which we need some help to do.
  ## The other thing we can do is when no link map is provided just
  ## knock them all out, I think.
  expect_equal(clean_whitespace(render_function(x, NULL)), ref)
})
