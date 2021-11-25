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
      private$cache <- list()
    },

    find = function(kind, name) {
      key <- paste(kind, name, sep = "|")
      if (key %in% names(private$cache)) {
        return(private$cache[[key]])
      }

      ## TODO: non-namespaced functions likely not to work here,
      ## unless we can find them some other way.
      if (kind == "function") {
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
        ## TODO: fix namespace here, need to find this!
        ret <- parse_function(xml, "dust::random")
      } else {
        stop("writeme")
      }

      private$cache[[key]] <- ret
      ret
    }
  ),

  private = list(
    path = NULL,
    index = NULL,
    index_members = NULL,
    cache = NULL
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
  definition <- xml2::xml_text(xml2::xml_find_first(x, "definition"))
  args <- xml2::xml_text(xml2::xml_find_first(x, "argsstring"))
  name <- xml2::xml_text(xml2::xml_find_first(x, "name"))

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
  ok <- length(kids) == 1 && xml2::xml_name(kids[[1]]) %in% c("ref", "text")
  if (!ok) {
    stop("computeroutput parse failure")
  }
  value <- xml2::xml_text(kids[[1]])
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
  tparam <- xml2::xml_find_first(x, "templateparamlist")
  if (xml_is_missing(x)) {
    return(NULL)
  }
  lapply(xml2::xml_find_all(tparam, "param/type"),
         linked_text)
}


simplify_text <- function(x) {
  if (length(x) < 2) {
    return(x)
  }
  is_text <- vcapply(x, "[[", "type") == "text"
  if (!any(is_text)) {
    return(x)
  }
  if (all(is_text)) {
    return(list(type = "text",
                value = paste(vcapply(x, "[[", "value"), collapse = "")))
  }
  browser()
}
