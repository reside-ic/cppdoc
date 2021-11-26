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


cppdoc_function <- function(name, args = NULL, package = NULL) {
  render_function(index_find(package, "function", name, args))
}


cppdoc_define <- function(name, package = NULL) {
  render_define(index_find(package, "define", name))
}


cppdoc_typedef <- function(name, package = NULL) {
  render_typedef(index_find(package, "typedef", name))
}


cppdoc_class <- function(name, package = NULL) {
  render_class(index_find(package, "class", name))
}


cppdoc_enum <- function(name, package = NULL) {
  render_typedef(index_find(package, "enum", name))
}


cppdoc_example <- function(name, input = TRUE, output = TRUE) {
  stop("writeme")
}
