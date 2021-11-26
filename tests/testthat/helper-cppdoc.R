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
  txt <- readLines(filename)
  re <- "cppdoc >> (.*)"
  i <- grep(re, txt)
  out <- unname(split(txt, findInterval(seq_along(txt), i)))

  f <- function(x) {
    stopifnot(grepl(re, x[[1]]),
              grepl("^---+\\s*$", x[[2]]))
    name <- sub(re, "\\1", x[[1]])
    value <- clean_whitespace(trimws(x[-(1:2)]))
    list(name = name, value = value)
  }

  res <- lapply(out, f)
  set_names(lapply(res, "[[", "value"),
            vcapply(res, "[[", "name"))
}


clean_whitespace <- function(x) {
  drop_trailing_whitespace(trimws(x))
}
