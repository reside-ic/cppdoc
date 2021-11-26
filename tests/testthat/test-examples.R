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
