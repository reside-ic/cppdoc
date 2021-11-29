## Markdown tests are much simpler, just test for the end result
## because the intermediate structures are not interesting.
test_that("can format markdown lists", {
  path <- doxygen_run_one("examples/markdown-itemizedlist.hpp")
  ref <- read_reference("examples/markdown-itemizedlist.txt")
  contents <- data.frame(kind = "function", name = "ex::f")
  x <- extract(path, NULL, contents)[[1L]]
  expect_equal(
    clean_whitespace(render_function(x)), ref)
})


test_that("Can apply basic formatting to markdown strings", {
  path <- doxygen_run_one("examples/markdown-formatting.hpp")
  ref <- read_reference("examples/markdown-formatting.txt")
  contents <- data.frame(kind = "function", name = "ex::f")
  x <- extract(path, NULL, contents)[[1L]]
  expect_equal(
    clean_whitespace(render_function(x)), ref)
})
