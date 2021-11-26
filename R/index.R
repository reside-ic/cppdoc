cppdoc_index <- function(path, quiet = FALSE, quiet_doxygen = TRUE) {
  message("Scanning vignettes for usage")
  path_vignettes <- file.path(path, "vignettes")
  ## So the rule would be look for vignettes that include
  ## 'cppdoc::cppdoc_register'
  files <- dir(path_vignettes, pattern = "\\.Rmd$")

  contents <- list()
  ## TODO: eventually track which goes where?
  ## TODO: verbose mode should print what we find and where
  for (f in files) {
    filename <- file.path(path_vignettes, f)
    txt <- readLines(filename)
    if (any(grepl("cppdoc::cppdoc_register", txt))) {
      contents <- c(contents, parse_rmd_cppdoc(filename))
    }
  }

  contents_df <- data.frame(
    kind = vcapply(contents, "[[", "kind"),
    name = vcapply(contents, "[[", "name"),
    args = I(lapply(contents, "[[", "args")))
  ## Don't yet support examples
  contents_df <- contents_df[contents_df$kind != "example", ]

  message("Running doxygen")
  doxygen_xml <- doxygen_run(path, quiet = quiet_doxygen)

  message(sprintf("Extracting %d definitions", nrow(contents_df)))
  value <- extract(doxygen_xml, contents_df)
  index <- cbind(contents_df, value = I(value))
  class(index) <- c("cppdoc_index", class(index))

  dest <- file.path(path, "inst/cppdoc/index.rds")
  dir.create(dirname(dest), FALSE, TRUE)

  message("Writing index")
  saveRDS(index, dest)
  invisible(index)
}


index_find <- function(package, kind, name, args = NULL) {
  if (is.null(package)) {
    package <- knitr::opts_chunk$get("cppdoc_package")
    if (is.null(package)) {
      stop("'package' not provided, but no default registered for knitr")
    }
  }
  if (is.character(package)) {
    index <- index_load(package)
  } else if (inherits(index, "cppdoc_index")) {
    index <- package
  }

  i <- which(index$kind == kind & index$name == name)
  if (length(i) == 0) {
    stop(sprintf("Lookup failure did not find %s '%s'", kind, name))
  }

  if (length(i) == 1 && length(args) == 0) {
    return(index$value[[i]])
  }

  if (length(i) > 1 && is.null(args)) {
    stop(sprintf("Lookup failure: disambiguate %s '%s'", kind, name))
  }

  j <- i[vcapply(index$args[i], normalise_arglist) == normalise_arglist(args)]
  if (length(j) != 1) {
    stop(sprintf("Lookup failure did not find %s '%s(%s)'",
                 kind, name, paste(args, collapse = ", ")))
  }

  index$value[[j]]
}


index_load <- function(package, reload = FALSE) {
  if (reload || !(package %in% names(projects))) {
    path <- system.file("cppdoc/index.rds", package = package, mustWork = TRUE)
    projects[[package]] <- readRDS(path)
  }
  projects[[package]]
}


projects <- new.env(parent = emptyenv())
