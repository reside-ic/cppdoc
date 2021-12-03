index_search_rmd <- function(path, quiet) {
  msg("Scanning .Rmd files for usage", quiet)
  files <- dir(path, pattern = "\\.Rmd$", recursive = TRUE)

  ## I don't think we need to do this, but best to be sure:
  files <- sub("\\", "/", files, fixed = TRUE)

  contents <- list()
  for (f in files) {
    filename <- file.path(path, f)
    txt <- readLines(filename)
    if (any(grepl("cppdoc::cppdoc_register", txt))) {
      x <- parse_rmd_cppdoc(txt)
      msg(sprintf("  - found %d entries in '%s'", nrow(x), f), quiet)
      contents[[f]] <- x
    }
  }

  n <- vapply(contents, nrow, integer(1))
  ret <- do.call("rbind", contents)
  rownames(ret) <- NULL

  ## Here we need to check for duplicates:
  if (any(duplicated(ret))) {
    stop("Need to deal with duplicate entries in index")
    ## If we have multiple given, then all but one needs to be marked
    ## as 'nolink' or similar so that we know not to link to it.  I
    ## don't think this is a huge deal.  We could also deal with the
    ## unique bits later.
  }

  msg(sprintf("  - total %d unique entries", nrow(ret)), quiet)
  ret$filename <- rep(names(contents), n)

  ret
}


parse_rmd_cppdoc <- function(txt) {
  ## It should be possible to better here with xpath, but not sure how
  ## to make that work because of namespacing weordness
  xml <- xml2::read_xml(commonmark::markdown_xml(txt, sourcepos = TRUE))
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

  data.frame(
    kind = vcapply(out$get(), "[[", "kind"),
    name = vcapply(out$get(), "[[", "name"),
    args = I(lapply(out$get(), "[[", "args")))
}


parse_rmd_cppdoc_entry <- function(x) {
  if (!is.recursive(x)) {
    stop(sprintf("Invalid statement '%s' within cppdoc block", deparse(x)))
  }
  fn <- as.character(x[[1]])
  if (!grepl("^cppdoc_", fn)) {
    stop(sprintf("Invalid function '%s' within cppdoc block", fn))
  }
  kind <- sub("^cppdoc_", "", fn)

  env <- environment()
  args <- match.call(match.fun(fn), x, envir = env)

  if (!is.character(args$name)) {
    stop(sprintf("'name' must be character in call to '%s'", fn))
  }
  args <- lapply(args[-1L], eval, baseenv())

  c(list(kind = kind), args)
}
