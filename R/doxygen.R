doxygen_locate <- function() {
  ## We should accept some envvar for doxygen here
  path <- unname(Sys.which("doxygen"))
  if (!nzchar(path)) {
    stop("Did not locate doxygen")
  }
  path
}


doxygen_run <- function(path, dest = tempfile(), quiet = FALSE,
                        standalone = FALSE) {
  bin <- doxygen_locate()
  dest <- tempfile()
  if (standalone) {
    package <- "cppdoc"
    path_include <- path
  } else {
    package <- read.dcf(file.path(path, "DESCRIPTION"), "Package")[[1]]
    path_include <- file.path(path, "inst/include")
  }
  cfg <- doxygen_configuration(package, path_include, dest)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(cfg, tmp)

  output <- if (quiet) FALSE else ""

  code <- system2(bin, tmp, stdout = output, stderr = output)
  if (code != 0) {
    stop("Error running doxygen")
  }

  dest
}


doxygen_configuration <- function(package, path, dest) {
  cfg <- list(project_name = package,
              output_directory = dest,
              extract_all = "YES",
              input = normalizePath(path, mustWork = TRUE),
              file_patterns = "*.hpp",
              recursive = "YES",
              generate_html = "NO",
              generate_latex = "NO",
              generate_xml = "YES",
              xml_output = ".",
              have_dot = "NO")
  sprintf("%s = %s", toupper(names(cfg)), vcapply(cfg, identity))
}
