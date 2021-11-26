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
  ## The doxygen output for dust here gives me an unexpected duplicate
  ## for one enum, which can be removed by using unique() below.  This
  ## is not needed on all versions (seen on 1.8.19 but not 1.8.17)
  members <- unique(members)
  rownames(members) <- NULL

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


parse_function <- function(x, name) {
  tparam <- parse_templateparamlist(
    xml2::xml_find_first(x, "templateparamlist"))

  value <- linked_text(xml2::xml_find_first(x, "type"))
  ## definition and args somehow optional here, don't really
  ## understand how.
  definition <- parse_definition(xml2::xml_find_first(x, "definition"))
  args <- parse_argsstring(xml2::xml_find_first(x, "argsstring"))
  name <- name %||% parse_name(xml2::xml_find_first(x, "name"))

  param <- lapply(xml2::xml_find_all(x, "param"), parse_param)

  ## Then the usage:
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))

  ## TODO: not covered here:
  ## * inbodydescription
  ## * location

  list(tparam = tparam,
       name = name,
       value = value,
       param = param,
       args = args,
       brief = brief,
       detail = detail)
}


parse_typedef <- function(x, name) {
  tparam <- parse_templateparamlist(
    xml2::xml_find_first(x, "templateparamlist"))
  definition <- parse_definition(xml2::xml_find_first(x, "definition"))
  args <- parse_argsstring(xml2::xml_find_first(x, "argsstring"))
  name <- name %||% parse_name(xml2::xml_find_first(x, "name"))
  type <- linked_text(xml2::xml_find_first(x, "type"))
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))

  ## We need to derive another form of the usage:
  stopifnot(grepl("^using", definition))
  definition_short <- sprintf("using %s = %s", name, type)
  ## otherwise sprintf("typedef %s %s", type, name)

  list(tparam = tparam,
       name = name,
       definition = definition,
       definition_short = definition_short,
       args = args,
       brief = brief,
       detail = detail)
}


parse_enum <- function(x, name) {
  name <- name %||% parse_name(xml2::xml_find_first(x, "name"))
  enumvalues <- lapply(xml2::xml_find_all(x, "enumvalue"),
                       parse_enumvalue)
  brief <- parse_description(xml2::xml_find_first(x, "briefdescription"))
  detail <- parse_description(xml2::xml_find_first(x, "detaileddescription"))
  strong <- xml2::xml_attr(x, "strong")

  list(name = name,
       strong = strong,
       enumvalues = enumvalues,
       brief = brief,
       detail = detail)
}


parse_define <- function(x, name) {
  name <- name %||% parse_name(xml2::xml_find_first(x, "name"))
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
parse_class <- function(x, name) {
  x <- xml2::xml_find_first(x, "compounddef")
  name <- name %||% parse_name(xml2::xml_find_first(x, "compoundname"))
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


## For now discarding link information, we can add it back later,
## though not sure how really
linked_text <- function(x) {
  vcapply(xml2::xml_contents(x), xml2::xml_text)
}


parse_description <- function(x) {
  kids <- xml2::xml_contents(x)
  nm <- xml2::xml_name(kids)
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
           bold = parse_bold(el),
           parameterlist = parse_parameterlist(el),
           simplesect = parse_simplesect(el),
           itemizedlist = parse_itemizedlist(el),
           ## there's also orderedlist to work with here, v similar
           stop(sprintf("unsupported tag '%s'", tag)))
  }

  kids <- xml2::xml_contents(x)
  ret <- lapply(kids, f)
  list(type = "para", value = simplify_text(ret))
}


parse_text <- function(x) {
  list(type = "text", value = xml2::xml_text(x))
}


parse_computeroutput <- function(x) {
  parse_markup(x, "`", "computeroutput")
}


parse_bold <- function(x) {
  parse_markup(x, "**", "bold")
}


parse_ref <- function(x) {
  list(type = "text", value = xml2::xml_text(x))
}


parse_markup <- function(x, md, name) {
  ## Similar to parse_para
  f <- function(el) {
    tag <- xml2::xml_name(el)
    switch(tag,
           text = parse_text(el),
           ref = parse_ref(el),
           stop(sprintf("unsupported tag '%s' within markup '%s'", tag, name)))
  }

  kids <- xml2::xml_contents(x)
  value <- simplify_text(lapply(kids, f))
  if (length(value) == 1 && value[[1]]$type == "text") {
    value[[1]]$value <- sprintf("%s%s%s", md, value[[1]]$value, md)
    return(value[[1]])
  }

  stop("Handle complex markup")
}


parse_parameterlist <- function(x) {
  kids <- xml2::xml_children(x) # can only be items
  value <- lapply(kids, parse_parameteritem)
  kind <- xml2::xml_attr(x, "kind")
  list(type = "parameters", value = value, kind = kind)
}


parse_simplesect <- function(x) {
  kind <- xml2::xml_attr(x, "kind")
  ## TODO: There might be a title here
  kids <- xml2::xml_children(x)
  if (!all(xml2::xml_name(kids) == "para")) {
    stop("Handle simplesect with title")
  }
  value <- lapply(kids, parse_para)
  list(type = "simplesect",
       kind = kind,
       value = value)
}


parse_itemizedlist <- function(x) {
  items <- lapply(xml2::xml_children(x), parse_listitem)
  list(type = "itemizedlist",
       items = items)
}


parse_listitem <- function(x) {
  lapply(xml2::xml_children(x), parse_para)
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
  ## TODO: This probably needs more work, unfortunately
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


parse_field <- function(x, name) {
  value <- linked_text(xml2::xml_find_first(x, "type"))
  name <- name %||% parse_name(xml2::xml_find_first(x, "name"))
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
      ret[[length(ret) + 1]] <- list(
        type = "section",
        kind = kind,
        value = lapply(xml2::xml_children(section), parse_memberdef))
    }
  }
  ret
}


parse_memberdef <- function(x) {
  kind <- xml2::xml_attr(x, "kind")
  switch(kind,
         "typedef" = parse_typedef(x, NULL),
         "variable" = parse_field(x, NULL),
         "function" = parse_function(x, NULL))
}


parse_param <- function(x) {
  ## There could be more here, though I don't see them in our case:
  ## defvalue: default value
  nms <- xml2::xml_name(xml2::xml_contents(x))
  req <- c("declname", "type")
  opt <- "defval"
  ok <- all(req %in% nms) && length(setdiff(nms, c(req, opt))) == 0
  if (!ok) {
    stop("Generalise parse_param")
  }
  list(type = parse_type(xml2::xml_find_first(x, "type")),
       name = parse_declname(xml2::xml_find_first(x, "declname")))
}


parse_type <- function(x) {
  linked_text(x)
}


parse_declname <- function(x) {
  xml2::xml_text(x)
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
    return(list(join(x)))
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
