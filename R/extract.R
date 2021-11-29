## A little caching-xml reader since we're going to hit the same few
## files many times.
doxygen_contents <- R6::R6Class(
  "doxygen_contents",
  public = list(
    path = NULL,
    index = NULL,
    members = NULL,

    initialize = function(path) {
      self$path <- path
      self$index <- parse_index(path)
      self$members <- parse_index_members(self$index)
    },

    read = function(id) {
      if (id %in% names(private$cache)) {
        private$cache[[id]]
      }
      x <- xml2::read_xml(file.path(self$path, paste0(id, ".xml")))
      private$cache[[id]] <- x
      x
    }
  ),
  private = list(
    cache = list()
  ))


extract <- function(path, examples, contents) {
  doxygen <- doxygen_contents$new(path)

  ret <- vector("list", nrow(contents))

  contents$index <- seq_len(nrow(contents))
  key <- paste(contents$kind, contents$name, sep = "\r")
  contents_split <- unname(split(contents, key))

  for (x in contents_split) {
    kind <- x$kind[[1]]
    name <- x$name[[1]]
    if (kind == "example") {
      ret[[x$index]] <- extract_example(examples, name)
    } else if (kind == "function") {
      ret[x$index] <- extract_function(doxygen, name, x$args)
    } else {
      ret[[x$index]] <- switch(kind,
                               "class" = extract_class(doxygen, name),
                               "define" = extract_define(doxygen, name),
                               "enum" = extract_enum(doxygen, name),
                               "typedef" = extract_typedef(doxygen, name),
                               stop(sprintf("Unknown kind '%s'", kind)))
    }
  }

  ret
}


extract_example <- function(examples, name) {
  ret <- examples[[name]]
  if (is.null(ret)) {
    stop(sprintf("Did not find example '%s'", name))
  }
  ret
}


extract_class <- function(doxygen, name) {
  index <- doxygen$index
  i <- check_index(index$name == name & index$kind == "class", "class", name)
  x <- doxygen$read(doxygen$index$refid[[i]])
  parse_class(x, name)
}


extract_define <- function(doxygen, name) {
  i <- find_member_index(doxygen, "define", name)
  x <- extract_member(doxygen, i)
  parse_define(x, name)
}


extract_enum <- function(doxygen, name) {
  i <- find_member_index(doxygen, "enum", name)
  x <- extract_member(doxygen, i)
  parse_enum(x, name)
}


extract_typedef <- function(doxygen, name) {
  i <- find_member_index(doxygen, "typedef", name)
  x <- extract_member(doxygen, i)
  parse_typedef(x, name)
}


extract_function <- function(doxygen, name, args) {
  i <- find_member_index(doxygen, "function", name)
  ret <- lapply(i, function(j)
    parse_function(extract_member(doxygen, j), name))

  ## Simple case, no match required:
  simple <- length(ret) == 1 &&
    (length(args) == 0 || length(args) == 1 && length(args[[1]]) == 0)
  if (simple) {
    return(ret)
  }

  if (length(ret) > 1 && is.null(args)) {
    stop(sprintf(
      "Can't decide which '%s' overload you want; args needed to disambiguate",
      name))
  }

  ## This is going to be nasty, going to use a heuristic for now:
  found <- vcapply(ret, function(x)
    normalise_arglist(vcapply(x$param, "[[", "type")))
  requested <- vcapply(args, normalise_arglist, USE.NAMES = FALSE)

  j <- match(requested, found)

  if (any(is.na(j))) {
    stop(sprintf("Did not find function overload '%s(%s)'",
                 name, paste(args, collapse = ", ")))
  }

  ret[j]
}


extract_member <- function(doxygen, i) {
  refid <- doxygen$members$refid[[i]]
  member_refid <- doxygen$members$member_refid[[i]]
  x <- doxygen$read(refid)
  query <- sprintf('//memberdef[@id = "%s"]', member_refid)
  xml2::xml_find_first(x, query)
}


find_member_index <- function(doxygen, kind, name) {
  ## TODO: this will certainly fail for global functions etc.  We also
  ## don't support any of the fancy finding of variables that breathe
  ## supports.
  idx <- doxygen$members
  if (kind == "define") {
    name_full <- idx$member_name
  } else {
    name_full <- paste(idx$name, idx$member_name, sep = "::")
  }
  check_index(name_full == name & idx$member_kind == kind, kind, name)
}


check_index <- function(i, kind, name) {
  if (!any(i)) {
    stop(sprintf("Did not find %s '%s'", kind, name))
  }
  if (sum(i) > 1 && kind != "function") {
    stop(sprintf("Unexpected ambiguous match for %s '%s'", kind, name))
  }
  which(i)
}


normalise_arglist <- function(x) {
  paste(gsub(" ", "", x), collapse = "\r")
}
