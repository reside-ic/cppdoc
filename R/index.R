##' Build an index for a package.  This requires that the doxygen is
##' installed and available.
##'
##' @title Create package index
##'
##' @param path Path to the package, default is the current directory.
##'
##' @param quiet Logical, indicating if cppdoc informational messages
##'   should be suppressed.
##'
##' @param quiet_doxygen Logical, indicating if doxygen should be run
##'   quietly.  It's pretty verbose so is set to be quiet by default.
##'
##' @return Invisibly, the package index, which is a `data.frame` with
##'   class `cppdoc_index`.  It is also written into the package
##'   directory at `inst/cppdoc/index.rds`.
##'
##' @export
cppdoc_index_package <- function(path = ".", quiet = FALSE,
                                 quiet_doxygen = TRUE) {
  path_description <- file.path(path, "DESCRIPTION")
  path_include <- file.path(path, "inst/include")  
  if (!file.exists(path_description)) {
    stop("This does not look like a package")
  }
  if (!file.exists(path_include)) {
    stop("This package does not have any include files")
  }
  package <- read.dcf(file.path(path, "DESCRIPTION"), "Package")[[1]]

  contents <- index_search_rmd(path, quiet)

  ## Don't yet support examples, drop them here
  contents <- contents[contents$kind != "example", ]

  index <- index_build(path_include, package, contents, quiet, quiet_doxygen)

  dest <- file.path(path, "inst/cppdoc/index.rds")
  dir.create(dirname(dest), FALSE, TRUE)

  saveRDS(index, dest)
  invisible(index)
}


index_load <- function(package, reload = FALSE) {
  if (reload || !(package %in% names(cache$packages))) {
    path <- system.file("cppdoc/index.rds", package = package, mustWork = TRUE)
    cache$packages[[package]] <- readRDS(path)
  }
  cache$packages[[package]]
}


index_get <- function(object) {
  if (inherits(object, "cppdoc_index")) {
    object
  } else if (is.null(object)) {
    package <- knitr::opts_chunk$get("cppdoc_package")
    if (is.null(package)) {
      stop("'package' not provided, but no default registered for knitr")
    }
    index_load(package)
  } else if (is.character(object)) {
    index_load(package)
  } else {
    stop("Invalid object provided for index")
  }
}


## This is really very similar to code within extract_function and
## friends but the underlying data structure is different.
##
## TODO: rationalise these, particularly as it's just the function
## overload bits that differ.
index_find <- function(index, kind, name, args = NULL) {
  index <- index_get(index)

  i <- which(index$kind == kind & index$name == name)
  if (length(i) == 0) {
    stop(sprintf("Lookup failure did not find %s '%s'", kind, name))
  }

  if (length(i) == 1 && is.null(args)) {
    return(index$value[[i]])
  }

  if (length(i) > 1 && is.null(args)) {
    stop(sprintf(
      "Can't decide which '%s' overload you want; args needed to disambiguate",
      name))
  }

  j <- i[vcapply(index$args[i], normalise_arglist) == normalise_arglist(args)]
  if (length(j) != 1) {
    stop(sprintf("Did not find function overload '%s(%s)'",
                 name, paste(args, collapse = ", ")))
  }

  index$value[[j]]
}


index_search_rmd <- function(path, quiet) {
  msg("Scanning .Rmd files for usage", quiet)
  files <- dir(path, pattern = "\\.Rmd$", recursive = TRUE)

  contents <- collector_list()
  ## TODO: eventually should track track which goes where, though this
  ## requires additional work to work out which are ones we point at
  ## for cross-referencing.
  for (f in files) {
    filename <- file.path(path, f)
    txt <- readLines(filename)
    if (any(grepl("cppdoc::cppdoc_register", txt))) {
      x <- parse_rmd_cppdoc(filename)
      msg(sprintf("  - found %d entries in '%s'", length(x), f), quiet)
      contents$append(x)
    }
  }

  ret <- data.frame(
    kind = vcapply(contents$get(), "[[", "kind"),
    name = vcapply(contents$get(), "[[", "name"),
    args = I(lapply(contents$get(), "[[", "args")))
  ret <- unique(ret)
  rownames(ret) <- NULL
  msg(sprintf("  - total %d unique entries", nrow(ret)), quiet)
  ret
}


index_build <- function(path, package, contents, quiet, quiet_doxygen) {
  msg("Running doxygen", quiet)
  doxygen_xml <- doxygen_run(path, package, quiet = quiet_doxygen)
  msg(sprintf("Extracting %d definitions", nrow(contents)), quiet)
  value <- extract(doxygen_xml, contents)
  index <- cbind(contents, value = I(value))
  class(index) <- c("cppdoc_index", class(index))
  index
}
