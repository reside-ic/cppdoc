doxygen_locate <- function() {
  path <- Sys.getenv("CPPDOC_DOXYGEN", unname(Sys.which("doxygen")))
  if (!nzchar(path)) {
    stop("Did not locate doxygen")
  }
  path
}


doxygen_run <- function(path, name, dest = tempfile(), quiet = FALSE) {
  bin <- doxygen_locate()

  cfg <- doxygen_configuration(name, path, dest)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(cfg, tmp)

  output <- if (quiet) FALSE else ""

  code <- system2(bin, tmp, stdout = output, stderr = output)
  if (code != 0) {
    ## TODO: at this point of course we've lost our logs.  Consider
    ## the system3 we have somewhere for this problem.
    stop("Error running doxygen")
  }

  dest
}


doxygen_configuration <- function(name, path, dest) {
  cfg <- list(project_name = name,
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
