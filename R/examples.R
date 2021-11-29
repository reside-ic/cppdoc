cppdoc_examples_run <- function(path) {
  package <- check_package(path)
  path_include <- check_package_include(path)

  path_examples <- file.path(path, "inst/cppdoc/examples")
  files <- dir(path_examples, pattern = "\\.cpp$", full.names = TRUE)
  res <- list()
  message(sprintf("Found %d examples in %s", length(files), package))
  for (f in files) {
    message(sprintf("Running '%s'", basename(f)))
    res <- example_run(f, path_include)
    saveRDS(res, sub("\\.cpp$", ".rds", f))
  }
}


example_run <- function(filename, path_include) {
  input <- readLines(filename)
  path_include <- normalizePath(path_include, mustWork = TRUE)

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file_copy(filename, tmp)

  src <- basename(filename)
  exe <- sub("\\.[[:alnum:]]+$", "", basename(filename))

  ## We can't use system2 here because the cxx command is already a
  ## composite command (probably something like `c++ --std=c++11`) so
  ## we have to use system and do our own quoting.
  ##
  ## We use -O0 because compilation speed will generally be limiting
  ## compared with running speed.
  cmd <- sprintf("%s -I%s -O0 -o%s %s",
                 cxx(), shQuote(path_include), shQuote(exe), shQuote(src))
  result <- with_dir(
    tmp,
    system(cmd, intern = TRUE))

  ## TODO: cope here on failure; we'll get a warning and an attribute
  ## that indicates what went wrong.  There will possibly be some
  ## output too.
  output <- with_dir(
    tmp,
    system2(sprintf("./%s", exe), stdout = TRUE, stderr = TRUE))

  list(input = input, output = output)
}


cxx <- function() {
  if (is.null(cache$cxx)) {
    cache$cxx <- system2("R", c("CMD", "config", "CXX"), stdout = TRUE)
  }
  cache$cxx
}
