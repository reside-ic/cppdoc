##' Create markdown output from C++ entities.
##'
##' @title Create markdown output
##'
##' @param name The name of the entity to render
##'
##' @param args For `cppdoc_function`, optionally argument types (not
##'   optional in the case of overloaded functions where this is used
##'   to select the appropriate method).
##'
##' @param package Optionally the package name.  This can be `NULL`
##'   after running `cppdoc_register`, typically within a knitr
##'   document.
##'
##' @return A character vector with markdown-formatted text, typically
##'   for inclusion in a knitr document via the `cppdoc` engine
##'
##' @export
##' @rdname cppdoc_markdown
cppdoc_function <- function(name, args = NULL, package = NULL) {
  index <- index_get(package)
  control <- cppdoc_control(index)
  render_function(index_find(index, "function", name, args), control)
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_define <- function(name, package = NULL) {
  index <- index_get(package)
  control <- cppdoc_control(index)
  render_define(index_find(package, "define", name), control)
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_typedef <- function(name, package = NULL) {
  index <- index_get(package)
  control <- cppdoc_control(index)
  render_typedef(index_find(package, "typedef", name), control)
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_class <- function(name, package = NULL) {
  index <- index_get(package)
  control <- cppdoc_control(index)
  render_class(index_find(package, "class", name), control)
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_enum <- function(name, package = NULL) {
  index <- index_get(package)
  control <- cppdoc_control(index)
  render_enum(index_find(package, "enum", name), control)
}


##' @export
##' @rdname cppdoc_markdown
##'
##' @param input,output Logical, indicating if input or output should
##'   be produced.
cppdoc_example <- function(name, input = TRUE, output = TRUE, package = NULL) {
  "**example support coming soon**"
}


cppdoc_control <- function(index, current = NULL) {
  if (is.null(current)) {
    current <- current_src()
  }
  list(link = index_compute_links(index, current))
}


current_src <- function() {
  ## The use of cache$current_input is a hack to support; we might do
  ## similar with the mode?
  path <- cache$current_input %||% knitr::current_input(TRUE)

  if (is.null(path)) {
    return(NULL)
  }

  if (!is.null(cache$current_mode)) {
    mode <- cache$current_mode
  } else if (identical(Sys.getenv("IN_PKGDOWN"), "true")) {
    mode <- "pkgdown"
  } else {
    mode <- "knitr"
  }

  ## This is a heuristic which should match reasonably well the way
  ## pkgdown works really.
  if (basename(dirname(path)) == "vignettes") {
    filename <- file.path("vignettes", basename(path))
  } else {
    filename <- basename(path)
  }

  list(filename = filename, mode = mode)
}
