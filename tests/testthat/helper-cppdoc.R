doxygen_run_one <- function(path, quiet = TRUE) {
  skip_if_no_doxygen()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  file.copy(path, tmp)
  doxygen_run(tmp, "cppdoc", quiet = quiet)
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


test_index <- function() {
  skip_if_no_doxygen()
  if (is.null(cache$test_index)) {
    contents <- read.csv("contents.csv", stringsAsFactors = FALSE)
    contents_args <- strsplit(contents$args, ";\\s*")
    contents$args <- I(rep(list(NULL), nrow(contents)))
    nms <- contents$name[contents$kind == "function"]
    i <- contents$name %in% nms[duplicated(nms)] & contents$kind == "function"
    contents$args[i] <- contents_args[i]
    cache$test_index <- index_build("examples_hpp", "examples_cpp", "cpptest",
                                    contents, TRUE, TRUE)
  }
  cache$test_index
}


has_doxygen <- function() {
  cache$has_doxygen <-
    !is.null(tryCatch(doxygen_locate(), error = function(e) NULL))
}


skip_if_no_doxygen <- function() {
  testthat::skip_if_not(has_doxygen(), "doxygen not found")
}


test_link_control <- function(path) {
  ## So what we want to say here is that the root type exists on this page
  idx <- doxygen_contents$new(path)

  ## So what we could do here is remap all these ids so that they are
  ## at least reproducible.  This would be better if we sought out the
  ## things we actually cared about because then it would be properly
  ## reproducible.  We'll sort that out later though.
  re <- "^(.+)([[:xdigit:]]{6})([[:xdigit:]]{26})$"
  ids_from <- c(idx$index$refid, idx$members$member_refid)
  i <- grep(re, ids_from)
  ids_to <- ids_from
  ids_to[i] <- sub(re, "\\1\\2", ids_from[i])
  list(
    page = "index.html",
    link = data.frame(
      refid = ids_from,
      page = "index.html",
      id = ids_to,
      stringsAsFactors = FALSE))
}
