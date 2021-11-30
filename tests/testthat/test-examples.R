test_that("can read simple typedef", {
  path <- doxygen_run_one("examples_hpp/typedef-simple.hpp")
  ref <- read_reference("examples_hpp/typedef-simple.txt")
  contents <- data.frame(kind = "typedef", name = "ex::real_type")
  x <- extract(path, NULL, contents)[[1L]]

  expect_null(x$tparam)
  expect_equal(x$name, "ex::real_type")
  expect_equal(x$definition_short, "using ex::real_type = double")
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
  expect_equal(x$definition_short, "using ex::int_type = int")
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
  expect_equal(x$value, "double")
  expect_equal(x$param, list(list(type = "double", name = "x"),
                             list(type = "double", name = "y")))
  expect_equal(x$args, "(double x, double y)")
  expect_null(x$brief)

  expect_equal(clean_whitespace(render_function(x, NULL)), ref)
})


test_that("Can render a templated function", {
  path <- doxygen_run_one("examples_hpp/function-templated.hpp")
  ref <- read_reference("examples_hpp/function-templated.txt")
  contents <- data.frame(kind = "function", name = "ex::generic")
  x <- extract(path, NULL, contents)[[1]]

  expect_equal(x$tparam, list("typename T", "typename U"))
  expect_equal(x$name, "ex::generic")
  expect_equal(x$value, "T")
  expect_equal(x$param, list(list(type = "const T &", name = "x"),
                             list(type = "U", name = "y")))
  ## TODO: would be nice to get the exact whitespace here
  expect_equal(x$args, "(const T &x, U y)")
  expect_null(x$brief)

  expect_equal(clean_whitespace(render_function(x, NULL)), ref)
})


test_that("can read simple define", {
  path <- doxygen_run_one("examples_hpp/define-simple.hpp")
  ref <- read_reference("examples_hpp/define-simple.txt")
  contents <- data.frame(kind = "define", name = "CONSTANT")
  x <- extract(path, NULL, contents)[[1]]

  expect_equal(x$name, "CONSTANT")
  expect_equal(x$value, "1")
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

  ## We need to do something here to remap links.
  ##
  ## The core class ends here ends up as
  ## > 'classex_1_1has__link'
  ## and the
  ## real_type > classex_1_1has__link_1a02a32e672d966804d3d3d17c8e6bae64
  ## f         > classex_1_1has__link_1a0a84fcb43ff68b8343502941d7fcaf89
  ## g         > classex_1_1has__link_1a7e95c4adf201b650bc42b9e10bdc1057
  ##
  ## These values might be hashes? in any case they're pretty
  ## annoying, and I don't see how determinstic they will be
  ##
  ## We can easily enough derive sensible names if we need to in the
  ## index, I think.
  ##   class-ex-has_link-type-real_type
  ##   class-ex-has_link-method-f
  ##   class-ex-has_link-method-g
  ## Where methods are overloaded it's a bit harder but we could just
  ## number them really.

  ## So what we want to say here is that the root type exists on this page
  idx <- doxygen_contents$new(path)

  ## So what we could do here is remap all these ids so that they are
  ## at least reproducible.  This would be better if we sought out the
  ## things we actually cared about because then it would be properly
  ## reproducible.  We'll sort that out later though.
  re <- "^(.+)([[:xdigit:]]{32})$"
  ids_from <- c(idx$index$refid, idx$members$member_refid)
  i <- grep(re, ids_from)
  ids_to <- ids_from
  ids_to[i] <- paste0(sub(re, "\\1", ids_from[i]), seq_along(i))
  ids_to <- paste0("#", ids_to) # all relative now
  control <- list(
    link = data.frame(from = ids_from, to = ids_to, stringsAsFactors = FALSE))

  x <- extract(path, NULL, contents)[[1]]
  render_class(x, control)

  expect_equal(x$name, "ex::has_link")

  expect_equal(clean_whitespace(render_class(x, NULL)), ref)
})


test_that("function attributes", {
  path <- doxygen_run_one("examples_hpp/function-attributes.hpp")
  contents <- data.frame(kind = "function", name = "ex::add_gpu")
  x <- extract(path, NULL, contents)[[1]]
  render_function(x, NULL)

  expect_equal(x$name, "ex::add_gpu")

  ## We need to rewrite links here, which we need some help to do.
  ## The other thing we can do is when no link map is provided just
  ## knock them all out, I think.
  expect_equal(clean_whitespace(render_class(x)), ref)
})
