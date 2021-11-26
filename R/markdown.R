parse_rmd_cppdoc <- function(path) {
  engines <- knitr::knit_engines$get()
  on.exit(knitr::knit_engines$restore(engines))

  ## This won't work well for any Rmd that sets its own engines. Of
  ## course we're going to go back through and fill things in shortly
  out <- collector_list()
  skip <- rep(list(function(options) NULL), length(engines))
  names(skip) <- names(engines)
  knitr::knit_engines$restore(skip)
  knitr::knit_engines$set(cppdoc = function(options) {
    for (el in parse(text = options$code)) {
      out$add(parse_rmd_cppdoc_entry(el))
    }
    NULL
  })
  knitr::knit(path, output = tempfile(), quiet = TRUE)

  ## TODO: this needs making more robust, I think, as we'll need to
  ## cope with errors with more reporting.
  out$get()
}


parse_rmd_cppdoc_entry <- function(x) {
  fn <- as.character(x[[1]])
  if (!grepl("^cppdoc_", fn)) {
    stop(sprintf("Invalid function '%s' within cppdoc block", fn))
  }
  kind <- sub("^cppdoc_", "", fn)

  env <- environment()
  args <- match.call(match.fun(fn), x, envir = env)

  if (!is.character(args$name)) {
    stop("'name' must be character in call to '%s'", fn)
  }
  args <- lapply(args[-1L], eval, baseenv())

  c(list(kind = kind), args)
}


cppdoc_function <- function(name, args = NULL) {
  stop("writeme")
}


cppdoc_define <- function(name) {
  stop("writeme")
}


cppdoc_typedef <- function(name) {
  stop("writeme")
}


cppdoc_class <- function(name, methods = TRUE) {
  stop("writeme")
}


cppdoc_enum <- function(name) {
  stop("writeme")
}


cppdoc_example <- function(name) {
  stop("writeme")
}
