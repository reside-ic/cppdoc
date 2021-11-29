doxygen_run_one <- function(path, quiet = TRUE) {
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
  if (is.null(cache$test_index)) {
    contents <- read.csv("examples/contents.csv", stringsAsFactors = FALSE)
    contents_args <- strsplit(contents$args, ";\\s*")
    contents$args <- I(rep(list(NULL), nrow(contents)))
    nms <- contents$name[contents$kind == "function"]
    i <- contents$name %in% nms[duplicated(nms)] & contents$kind == "function"
    contents$args[i] <- contents_args[i]
    cache$test_index <- index_build("examples", "cpptest", contents, TRUE, TRUE)
  }
  cache$test_index
}
