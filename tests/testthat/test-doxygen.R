test_that("informative error if doxygen not found", {
  testthat::skip_if_not_installed("mockery")
  mock_sys_which <- mockery::mock("")
  mockery::stub(doxygen_locate, "Sys.which", mock_sys_which)
  expect_error(doxygen_locate(),
               "Did not locate doxygen")
  mockery::expect_called(mock_sys_which, 1)
  expect_equal(mockery::mock_args(mock_sys_which)[[1]],
               list("doxygen"))
})


test_that("locate doxygen on path if found", {
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("withr")

  mock_sys_which <- mockery::mock("/path/to/doxygen", cycle = TRUE)
  mockery::stub(doxygen_locate, "Sys.which", mock_sys_which)
  expect_equal(doxygen_locate(), "/path/to/doxygen")

  withr::with_envvar(
    c("CPPDOC_DOXYGEN" = "/other/doxygen"),
    expect_equal(doxygen_locate(), "/other/doxygen"))
})


test_that("report on running error", {
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("withr")
  mock_system2 <- mockery::mock(-1)
  mockery::stub(doxygen_run, "system2", mock_system2)

  bin <- "/path/to/doxygen"
  dest <- "dest"
  expect_error(
    withr::with_envvar(
      c("CPPDOC_DOXYGEN" = bin),
      doxygen_run(".", "name", dest)),
    "Error running doxygen")
  mockery::expect_called(mock_system2, 1)

  args <- mockery::mock_args(mock_system2)[[1]]
  expect_equal(args[[1]], bin)
  expect_equal(args[3:4], list(stdout = "", stderr = ""))
})
