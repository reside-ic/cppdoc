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
  render_function(index_find(package, "function", name, args))
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_define <- function(name, package = NULL) {
  render_define(index_find(package, "define", name))
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_typedef <- function(name, package = NULL) {
  render_typedef(index_find(package, "typedef", name))
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_class <- function(name, package = NULL) {
  render_class(index_find(package, "class", name))
}


##' @export
##' @rdname cppdoc_markdown
cppdoc_enum <- function(name, package = NULL) {
  render_enum(index_find(package, "enum", name))
}


##' @export
##' @rdname cppdoc_markdown
##'
##' @param input,output Logical, indicating if input or output should
##'   be produced.
cppdoc_example <- function(name, input = TRUE, output = TRUE, package = NULL) {
  "**example support coming soon**"
}
