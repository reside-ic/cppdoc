##' Prepare for use with knitr by registering the `cppdoc` knitr
##' engine and optionally setting a default package to locate C++
##' definitions in.  This operation can be undone with
##' `cppdoc_unregister`.
##'
##' @title Prepare for use with knitr
##'
##' @param package Name of the package to use
##'
##' @return Nothing, called for its side effects
##'
##' @export
##' @examples
##' cppdoc::cppdoc_register(NULL)
##' cppdoc::cppdoc_unregister()
cppdoc_register <- function(package) {
  ret <- knitr::knit_engines$set(cppdoc = cppdoc_engine)
  if (!is.null(package)) {
    browser()
    index <- index_load(package)
    knitr::opts_chunk$set(cppdoc_package = package)
  }
  invisible(ret)
}


##' @rdname cppdoc_register
##' @export
cppdoc_unregister <- function() {
  knitr::knit_engines$delete("cppdoc")
  knitr::opts_chunk$delete("cppdoc_package")
}


cppdoc_engine <- function(options) {
  out <- collector()
  for (expr in parse(text = options$code)) {
    out$add(eval(expr, asNamespace("cppdoc")), TRUE)
  }
  paste(out$get(), collapse = "\n")
}
