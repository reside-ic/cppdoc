check_package <- function(path) {
  path_description <- file.path(path, "DESCRIPTION")
  if (!file.exists(path_description)) {
    stop("This does not look like a package")
  }
  read.dcf(file.path(path, "DESCRIPTION"), "Package")[[1]]
}


check_package_include <- function(path) {
  path_include <- file.path(path, "inst/include")
  if (!file.exists(path_include)) {
    stop("This package does not have any include files")
  }
  path_include
}


check_package_examples <- function(path) {
  check_package(path)
  path_examples <- file.path(path, "inst/cppdoc/examples")
  if (!file.exists(path_examples)) {
    path_examples <- NULL
  }
  path_examples
}
