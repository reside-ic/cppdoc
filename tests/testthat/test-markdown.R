test_that("Can render function", {
  index <- test_index()
  expect_equal(
    clean_whitespace(cppdoc_function("ex::add", package = index)),
    read_reference("examples/function-simple.txt"))
})


test_that("Can render define", {
  index <- test_index()
  expect_equal(
    clean_whitespace(cppdoc_define("CONSTANT", package = index)),
    read_reference("examples/define-simple.txt"))
})


test_that("Can render type", {
  index <- test_index()
  expect_equal(
    clean_whitespace(cppdoc_typedef("ex::real_type", package = index)),
    read_reference("examples/typedef-simple.txt"))
})


test_that("Can render class", {
  index <- test_index()
  expect_equal(
    clean_whitespace(cppdoc_class("ex::test", package = index)),
    read_reference("examples/class-simple.txt"))
})


test_that("Can render enum", {
  index <- test_index()
  expect_equal(
    clean_whitespace(cppdoc_enum("ex::fruit", package = index)),
    read_reference("examples/enum-simple.txt"))
})


test_that("Can render example", {
  index <- test_index()
  expect_equal(
    clean_whitespace(cppdoc_example("example", package = index)),
    "**example support coming soon**")
})
