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
  index <- extract(doxygen_xml, contents_df)

  dest <- file.path(path, "inst/cppdoc/index.rds")
  dir.create(dirname(dest), FALSE, TRUE)

  message("Writing index")
  saveRDS(index, dest)
}
