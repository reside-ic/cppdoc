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
