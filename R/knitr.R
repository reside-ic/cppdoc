cppdoc_engine <- function(options) {
  out <- collector()
  for (expr in parse(text = options$code)) {
    out$collect(
      eval(parse_rmd_cppdoc_entry(expr), asNamespace("cppdoc")),
      TRUE)
  }
  out$get()
}


cppdoc_register <- function(package) {
  knitr::knit_engines$set(cppdoc = cppdoc_engine)
  index_load(package)
  knitr::opts_chunk$set(cppdoc_package = package)
}
