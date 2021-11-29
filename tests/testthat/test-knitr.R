test_that("can always unregister knitr engine", {
  ## Ensure cleaned out (this is what happens in the function anyway!)
  knitr::knit_engines$delete("cppdoc")
  knitr::opts_chunk$delete("cppdoc_package")

  expect_null(knitr::knit_engines$get("cppdoc"))
  expect_null(knitr::opts_chunk$get("cppdoc_package"))
  expect_silent(cppdoc_unregister())
  expect_null(knitr::knit_engines$get("cppdoc"))
  expect_null(knitr::opts_chunk$get("cppdoc_package"))
})


test_that("register and unresgister", {
  cppdoc_register("cppdoc")
  expect_identical(knitr::knit_engines$get("cppdoc"), cppdoc_engine)
  expect_equal(knitr::opts_chunk$get("cppdoc_package"), "cppdoc")
  cppdoc_unregister()
  expect_null(knitr::knit_engines$get("cppdoc"))
  expect_null(knitr::opts_chunk$get("cppdoc_package"))
})
