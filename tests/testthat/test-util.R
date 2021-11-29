test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("msg prints messages only when asked", {
  expect_silent(msg("hello", TRUE))
  expect_message(msg("hello", FALSE), "hello")
})


test_that("unimplemented can assert", {
  expect_silent(unimplemented(FALSE, "something"))
  expect_error(
    unimplemented(TRUE, "something"),
    "cppdoc unimplemented: something")
})


test_that("copying files throws an error", {
  expect_error(file_copy(tempfile(), tempfile()),
               "Error copying files")
})
