test_that("can compile simple package", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  path_include <- file.path(tmp, "inst/include")
  path_vignettes <- file.path(tmp, "vignettes")
  path_index <- file.path(tmp, "inst/cppdoc/index.rds")

  expect_error(
    cppdoc_index_package(tmp, TRUE, TRUE),
    "This does not look like a package")

  file.create(file.path(tmp, "NAMESPACE"))
  writeLines(c("Package: cpptest.ex1",
               "Version: 0.0.1",
               "Licence: CC0"),
             file.path(tmp, "DESCRIPTION"))

  expect_error(
    cppdoc_index_package(tmp, TRUE, TRUE),
    "This package does not have any include files")

  dir.create(path_include, FALSE, TRUE)
  file.copy("examples/function-simple.hpp", path_include)

  expect_error(
    cppdoc_index_package(tmp, TRUE, TRUE),
    "Did not find any cppdoc usage in package 'cpptest.ex1'")

  dir.create(path_vignettes, FALSE, TRUE)
  file.copy("examples/simple.Rmd", path_vignettes)

  index <- cppdoc_index_package(tmp, TRUE, TRUE)
  expect_s3_class(index, "cppdoc_index")
  expect_equal(nrow(index), 1)
  expect_true(file.exists(path_index))
  expect_equal(readRDS(path_index), index)
})


test_that("can load a package index", {
  skip_if_not_installed("mockery")
  cache$packages <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp))
  saveRDS(test_index(), tmp)
  mock_system_file <- mockery::mock(tmp)
  mockery::stub(index_load, "system.file", mock_system_file)
  index <- index_load("thepkg")
  expect_identical(index, test_index())

  mockery::expect_called(mock_system_file, 1)
  expect_equal(
    mockery::mock_args(mock_system_file)[[1]],
    list("cppdoc/index.rds", package = "thepkg", mustWork = TRUE))
  expect_identical(cache$packages$thepkg, index)
})


test_that("can get indices", {
  cppdoc_unregister()
  cache$packages <- list()
  cache$packages[["thepkg"]] <- test_index()
  expect_identical(index_get("thepkg"), test_index())
  expect_error(
    index_get(NULL),
    "'package' not provided, but no default registered for knitr")
  cppdoc_register("thepkg")
  expect_identical(index_get(NULL), test_index())
  expect_error(index_get(TRUE), "Invalid object provided for index")
})
