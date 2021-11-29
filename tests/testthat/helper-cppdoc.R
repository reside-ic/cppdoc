doxygen_run_one <- function(path, quiet = TRUE) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  file.copy(path, tmp)
  doxygen_run(tmp, quiet = quiet, standalone = TRUE)
}


test_para <- function(text) {
  list(list(type = "text", value = text))
}


read_reference <- function(filename) {
  clean_whitespace(readLines(filename))
}


clean_whitespace <- function(x) {
  drop_trailing_whitespace(trimws(x))
}
