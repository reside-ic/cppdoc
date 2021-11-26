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
  render_typedef(index_find(package, "enum", name))
}


##' @export
##' @rdname cppdoc_markdown
##'
##' @param input,output Logical, indicating if input or output should
##'   be produced.
cppdoc_example <- function(name, input = TRUE, output = TRUE) {
  "** example support coming soon **"
}


parse_rmd_cppdoc <- function(path) {
  ## It should be possible to better here with xpath, but not sure how
  ## to make that work because of namespacing weordness
  xml <- xml2::read_xml(commonmark::markdown_xml(readLines(path),
                                                 sourcepos = TRUE))
  blocks <- xml2::xml_find_all(xml, "//d1:code_block")
  type <- xml2::xml_attr(blocks, "info")
  re <- "\\s*\\{\\s*cppdoc.*\\}"
  code <- lapply(blocks[grepl(re, type)], xml2::xml_text)

  ## TODO: better reporting here, ideally; see the sourcepos argument
  out <- collector_list()
  for (x in code) {
    for (el in parse(text = x)) {
      out$add(parse_rmd_cppdoc_entry(el))
    }
  }

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
