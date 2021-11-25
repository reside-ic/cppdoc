parse_index <- function(path) {
  index_xml <- xml2::read_xml(file.path(path, "index.xml"))
  elements <- lapply(xml2::xml_find_all(index_xml, "/doxygenindex/compound"),
                     parse_index_compound)
  data.frame(kind = vcapply(elements, "[[", "kind"),
             name = vcapply(elements, "[[", "name"),
             refid = vcapply(elements, "[[", "refid"),
             members = I(lapply(elements, "[[", "members")),
             stringsAsFactors = FALSE)
}


parse_index_compound <- function(x) {
  attrs <- xml2::xml_attrs(x)
  name <- xml2::xml_text(xml2::xml_find_first(x, "name"))
  members <- lapply(xml2::xml_find_all(x, "member"), parse_index_member)

  members <- data.frame(name = vcapply(members, "[[", "name"),
                        refid = vcapply(members, "[[", "refid"),
                        kind = vcapply(members, "[[", "kind"),
                        stringsAsFactors = FALSE)

  list(name = name,
       refid = attrs[["refid"]],
       kind = attrs[["kind"]],
       members = members)
}

parse_index_member <- function(x) {
  attrs <- xml2::xml_attrs(x)
  name <- xml2::xml_text(xml2::xml_find_first(x, "name"))
  list(name = name, refid = attrs[["refid"]], kind = attrs[["kind"]])
}


parse_index_members <- function(index) {
  members <- do.call(rbind, index$members)
  names(members) <- paste0("member_", names(members))
  n <- vapply(index$members, nrow, integer(1))
  i <- rep(seq_along(n), n)
  index_members <- cbind(index[i, c("kind", "name", "refid"), drop = FALSE],
                         members)
  rownames(index_members) <- NULL
  index_members
}


doxygen <- R6::R6Class(
  "doxygen",
  cloneable = FALSE,

  public = list(
    initialize = function(path) {
      private$path <- path
      private$index <- parse_index(path)
      private$index_members <- parse_index_members(private$index)
    },

    ## TODO: Probably work to get the file reads cached here, or just
    ## open all of them up
    find = function(kind, name) {
      ## TODO: non-namespaced functions likely not to work here,
      ## unless we can find them some other way.
      if (kind %in% c("function", "enum", "typedef")) {
        idx <- private$index_members
        name_full <- paste(idx$name, idx$member_name, sep = "::")
        i <- name_full == name & idx$member_kind == kind
        if (!any(i)) {
          stop(sprintf("Did not find %s '%s'", kind, name))
        }
        if (sum(i) > 1) {
          stop(sprintf("Disambiguate match for %s '%s'", kind, name))
        }
        d <- as.list(idx[which(i), ])
        xml <- extract_member(private$path, d$refid, d$member_refid)
        ret <- switch(kind,
                      "function" = parse_function(xml, d$name),
                      "enum" = parse_enum(xml, d$name),
                      "typedef" = parse_typedef(xml, d$name))
      } else if (kind == "define") {
        ## This is almost the same as above, but we change the name.
        ## If we tweak how name_full is computed we don't need it.
        idx <- private$index_members
        i <- idx$member_name == name & idx$member_kind == kind
        if (!any(i)) {
          stop(sprintf("Did not find %s '%s'", kind, name))
        }
        if (sum(i) > 1) {
          stop(sprintf("Disambiguate match for %s '%s'", kind, name))
        }
        d <- as.list(idx[which(i), ])
        xml <- extract_member(private$path, d$refid, d$member_refid)
        ret <- parse_define(xml)
      } else if (kind == "class") {
        ## The class handling is really quite different, because we
        ## need to build a complex thing.
        idx <- private$index
        i <- idx$name == name & idx$kind == kind
        if (!any(i)) {
          stop(sprintf("Did not find %s '%s'", kind, name))
        }
        if (sum(i) > 1) {
          stop(sprintf("Disambiguate match for %s '%s'", kind, name))
        }
        d <- as.list(idx[which(i), ])
        xml <- xml2::read_xml(file.path(private$path, paste0(d$refid, ".xml")))
        ret <- parse_class(xml)
      } else {
        stop("writeme")
      }

      ret
    }
  ),

  private = list(
    path = NULL,
    index = NULL,
    index_members = NULL
  ))


extract_member <- function(path, refid, member_refid) {
  xml <- xml2::read_xml(file.path(path, paste0(refid, ".xml")))
  query <- sprintf('//memberdef[@id = "%s"]', member_refid)
  xml2::xml_find_first(xml, query)
}


parse_function <- function(x, namespace) {
  tparam <- parse_templateparamlist(
    xml2::xml_find_first(x, "templateparamlist"))

  value <- linked_text(xml2::xml_find_first(x, "type"))
  ## definition and args somehow optional here, don't really
  ## understand how.
  definition <- parse_definition(xml2::xml_find_first(x, "definition"))
  args <- parse_argsstring(xml2::xml_find_first(x, "argsstring"))
  name <- parse_name(xml2::xml_find_first(x, "name"))

  ## Then the usage:
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))

  ## TODO: not covered here:
  ## * inbodydescription
  ## * location

  list(namespace = namespace,
       tparam = tparam,
       name = name,
       value = value,
       args = args,
       brief = brief,
       detail = detail)
}


parse_typedef <- function(x, namespace) {
  tparam <- parse_templateparamlist(
    xml2::xml_find_first(x, "templateparamlist"))
  definition <- parse_definition(xml2::xml_find_first(x, "definition"))
  args <- parse_argsstring(xml2::xml_find_first(x, "argsstring"))
  name <- parse_name(xml2::xml_find_first(x, "name"))
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))

  list(namespace = namespace,
       tparam = tparam,
       name = name,
       definition = definition,
       args = args,
       brief = brief,
       detail = detail)
}


parse_enum <- function(x, namespace) {
  name <- parse_name(xml2::xml_find_first(x, "name"))
  enumvalues <- lapply(xml2::xml_find_all(x, "enumvalue"),
                       parse_enumvalue)
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))

  list(namespace = namespace,
       name = name,
       enumvalues = enumvalues,
       brief = brief,
       detail = detail)
}


parse_define <- function(x) {
  name <- parse_name(xml2::xml_find_first(x, "name"))
  value <- linked_text(xml2::xml_find_first(x, "initializer"))
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))

  list(name = name,
       value = value,
       brief = brief,
       detail = detail)
}


parse_enumvalue <- function(x) {
  name <- parse_name(xml2::xml_find_first(x, "name"))
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))
  list(name = name, brief = brief, detail = detail)
}


## This is pretty poor, not coping with things like friends,
## inheritance, protected methods etc.
parse_class <- function(x) {
  x <- xml2::xml_find_first(x, "compounddef")
  name <- parse_name(xml2::xml_find_first(x, "compoundname"))
  tparam <- parse_templateparamlist(
    xml2::xml_find_first(x, "templateparamlist"))
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))
  sections <- parse_sections(xml2::xml_find_all(x, "sectiondef"))
  list(tparam = tparam,
       name = name,
       sections = sections,
       brief = brief,
       detail = detail)
}


xml_is_missing <- function(x) {
  inherits(x, "xml_missing")
}


xml_is_found <- function(x) {
  !xml_is_missing(x)
}


## For now discarding link information, we can add it back later,
## though not sure how really
linked_text <- function(x) {
  vcapply(xml2::xml_contents(x), xml2::xml_text)
}


parse_description <- function(x) {
  kids <- xml2::xml_contents(x)
  nm <- vcapply(kids, xml2::xml_name)
  if (all(nm == "text") && !nzchar(trimws(xml2::xml_text(x)))) {
    return(NULL)
  }
  stopifnot(all(nm == "para"))
  ret <- lapply(kids, parse_para)
  ret
}


parse_para <- function(x) {
  f <- function(el) {
    tag <- xml2::xml_name(el)
    switch(tag,
           text = parse_text(el),
           computeroutput = parse_computeroutput(el),
           parameterlist = parse_parameterlist(el),
           stop(sprintf("unsupported tag '%s'", tag)))
  }

  kids <- xml2::xml_contents(x)
  ret <- lapply(kids, f)
  simplify_text(ret)
}


parse_text <- function(x) {
  list(type = "text", value = xml2::xml_text(x))
}


parse_computeroutput <- function(x) {
  ## This is where we could handle links, so long as we build nice
  ## anchors
  kids <- xml2::xml_contents(x)
  type <- xml2::xml_name(kids)
  ok <- all(type %in% c("ref", "text"))
  if (!ok) {
    stop("computeroutput parse failure")
  }
  ## TODO: discards link information
  value <- paste(xml2::xml_text(kids), collapse = "")
  list(type = "text", value = sprintf("`%s`", value))
}


parse_parameterlist <- function(x) {
  kids <- xml2::xml_children(x) # can only be items
  value <- lapply(kids, parse_parameteritem)
  kind <- xml2::xml_attr(x, "kind")
  list(type = "parameters", value = value, kind = kind)
}


parse_parameteritem <- function(x) {
  ## the spec says that there could be zero-inf parameternamelist entries
  name <- parse_parameternamelist(
    xml2::xml_find_first(x, "parameternamelist"))
  description <- parse_description(
    xml2::xml_find_first(x, "parameterdescription"))
  list(name = name,
       description = description)
}


parse_parameternamelist <- function(x) {
  vcapply(xml2::xml_children(x), xml2::xml_text)
}


parse_templateparamlist <- function(x) {
  if (xml_is_missing(x)) {
    return(NULL)
  }
  lapply(xml2::xml_find_all(x, "param/type"),
         linked_text)
}


parse_argsstring <- function(x) {
  xml2::xml_text(x)
}


parse_definition <- function(x) {
  xml2::xml_text(x)
}


parse_name <- function(x) {
  xml2::xml_text(x)
}


parse_variable <- function(x) {
  value <- linked_text(xml2::xml_find_first(x, "type"))
  name <- parse_name(xml2::xml_find_first(x, "name"))
  args <- parse_argsstring(xml2::xml_find_first(x, "argsstring"))
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))
  list(name = name, value = value, args = args, brief = brief, detail = detail)
}


parse_sections <- function(x) {
  ret <- list()
  for (section in x) {
    kind <- xml2::xml_attr(section, "kind")
    if (grepl("^public-", kind)) {
      ret[[kind]] <- lapply(xml2::xml_children(section), parse_memberdef)
    }
  }
  ret
}


parse_memberdef <- function(x) {
  kind <- xml2::xml_attr(x, "kind")
  switch(kind,
         "typedef" = parse_typedef(x, NULL),
         "variable" = parse_variable(x, NULL),
         "function" = parse_function(x, NULL))
}


simplify_text <- function(x) {
  join <- function(x) {
    list(type = "text",
         value = paste(vcapply(x, "[[", "value"), collapse = ""))
  }
  if (length(x) < 2) {
    return(x)
  }
  is_text <- vcapply(x, "[[", "type") == "text"
  if (!any(is_text)) {
    return(x)
  }
  if (all(is_text)) {
    return(join(x))
  }

  ## The most complex case; collapse adjacent text blocks:
  tmp <- rle(is_text)
  i <- tmp$values & tmp$lengths > 1
  if (any(i)) {
    idx <- unname(split(seq_along(x), rep(seq_along(tmp$lengths), tmp$lengths)))
    for (j in which(i)) {
      k <- idx[[j]]
      x[[k[[1]]]] <- join(x[k])
    }
    x <- x[-unlist(lapply(idx[i], "[", -1))]
  }

  x
}
