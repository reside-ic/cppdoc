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
cppdoc_index_package <- function(path = ".",
                                 quiet = FALSE,
                                 quiet_doxygen = TRUE) {
  package <- check_package(path)
  path_include <- check_package_include(path)
  path_examples <- check_package_examples(path)

  contents <- index_search_rmd(path, quiet)

  if (nrow(contents) == 0) {
    stop(sprintf("Did not find any cppdoc usage in package '%s'", package))
  }

  index <- index_build(path_include, path_examples, package,
                       contents, quiet, quiet_doxygen)

  dest <- file.path(path, "inst/cppdoc/index.rds")
  dir.create(dirname(dest), FALSE, TRUE)

  saveRDS(index, dest)
  invisible(index)
}


index_load <- function(package) {
  ## TODO: better error if loading not possible, this is R's silly
  ## error
  if (!(package %in% names(cache$packages))) {
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
      ## This error designed to work with the functions in markdown.R
      stop("'package' not provided, but no default registered for knitr")
    }
    index_load(package)
  } else if (is.character(object)) {
    index_load(object)
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


index_build <- function(path_include, path_examples, package,
                        contents, quiet, quiet_doxygen) {
  msg("Running doxygen", quiet)
  doxygen_xml <- doxygen_run(path_include, package, quiet = quiet_doxygen)
  examples <- cppdoc_examples_run(path_examples, path_include, package, quiet)

  i <- duplicated(contents[c("kind", "name", "args")])
  if (any(i)) {
    stop("Fix duplicates")
  }

  msg(sprintf("Extracting %d definitions", nrow(contents)), quiet)
  value <- extract(doxygen_xml, examples, contents)
  index <- cbind(contents,
                 value = I(value),
                 id = vcapply(value, function(x) x$id %||% NA_character_))

  ## This needs sorting out; see below
  if (is.null(index$filename)) {
    index$filename <- NA_character_
  }

  class(index) <- c("cppdoc_index", class(index))
  index
}


## We might move this into the render actually because this is more
## something that is not intrinsic to the index.
index_target <- function(filename, mode) {
  ## pkgdown: translate
  ## * vignettes/<path>.Rmd -> articles/<path>.html
  ## * <immediate-path>.Rmd -> <immidiate-path>.Rmd
  ## knitr: translate
  ## * vignettes/<path>.Rmd -> vignettes/<path>.html
  ## * <immediate-path>.Rmd -> NA
  re_vignette <- "^vignettes/(.+)\\.Rmd$"
  re_toplevel <- "^([^/]+)\\.Rmd$"

  is_vignette <- grepl(re_vignette, filename)
  is_toplevel <- grepl(re_toplevel, filename)

  target <- rep(NA_character_, length(filename))
  if (mode == "pkgdown") {
    target[is_vignette] <-
      sub(re_vignette, "articles/\\1.html", filename[is_vignette])
    target[is_toplevel] <-
      sub(re_toplevel, "\\1.html", filename[is_toplevel])
  } else {
    target[is_vignette] <-
      sub(re_vignette, "vignettes/\\1.html", filename[is_vignette])
  }

  target
}


index_compute_links <- function(index, current) {
  if (is.null(current)) {
    return(NULL)
  }
  ## This bit of logic works out the links, based on the current page:
  ##
  ## Consider rmarkdown::relative_to, might work ok
  ##
  ## TODO: what about missing paths here?  We return NULL in some
  ## places and path_rel might not like that.
  mode <- current$mode
  path <- as.character(fs::path_rel(index_target(index$filename, mode),
                                    index_target(current$filename, mode)))
  path[path == "."] <- ""
  link <- data.frame(id = index$id, path = path, stringsAsFactors = FALSE)
  link[complete.cases(link), ]
}
