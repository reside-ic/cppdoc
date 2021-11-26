render_entry <- function(x, kind) {
  switch(kind,
         "function" = render_function(x$name, x$args),
         "define" = render_define(x$name),
         "enum" = render_enum(x$name),
         "typedef" = render_typedef(x$name),
         "class" = render_class(x$name),
         "example" = render_example(x$name))
}


render_function <- function(x) {
  out <- collector()

  decl <- paste0(x$name, x$args)
  if (length(x$value) > 0) {
    decl <- paste(paste(trimws(x$value), collapse = " "), decl)
  }

  out$add(md_code_block(c(render_tparam(x$tparam), decl)))

  out$add(render_brief(x$brief), TRUE)
  out$add(render_detail(x$detail), TRUE)

  out$get()
}


render_define <- function(x) {
  if (!is.null(x$brief)) {
    stop("Write rendering for brief")
  }
  if (!is.null(x$detail)) {
    stop("Write rendering for detail")
  }

  md_code_block(sprintf("#define %s %s", x$name, x$value))
}


render_enum <- function(x) {
  if (!is.null(x$brief)) {
    stop("Write rendering for brief")
  }
  if (!is.null(x$detail)) {
    stop("Write rendering for detail")
  }
  ## TODO: I *think* but am not sure that 'enum class' vs 'enum'
  ## changes the "strong" field here.
  type <- if (x$strong == "yes") "enum class" else "enum"
  out <- collector()
  out$add(sprintf("%s %s", type, x$name))

  if (length(x$enumvalues) > 0) {
    out$add("")
    out$add(md_bold("Values"))
  }
  for (el in x$enumvalues) {
    out$add("")
    out$add(render_enumvalue(el))
  }

  out$get()
}


render_typedef <- function(x) {
  out <- collector()
  out$add(md_code_block(c(
    render_tparam(x$tparam),
    x$definition_short)))

  out$add(render_brief(x$brief), TRUE)
  out$add(render_detail(x$detail), TRUE)

  out$get()
}


## TODO: Looks like we're inconsistent throughout as to if we pass
## $value or not, would be good to check that.
render_para <- function(x) {
  out <- collector()
  if (x$type != "para") {
    stop("check this")
  }
  for (el in x$value) {
    value <- switch(el$type,
                    "text" = el$value,
                    "parameters" = render_parameters(el),
                    "simplesect" = render_simplesect(el),
                    "itemizedlist" = render_itemizedlist(el),
                    stop(sprintf("Unsupported type '%s'", el$type)))
    out$add(value, TRUE)
  }
  out$get()
}


render_class <- function(x) {
  out <- collector()
  out$add(md_code_block(c(render_tparam(x$tparam),
                          paste("class", x$name))))

  out$add(render_brief(x$brief), TRUE)
  out$add(render_detail(x$detail), TRUE)

  for (s in x$sections) {
    out$add(render_section(s), TRUE)
  }

  out$get()
}


render_paras <- function(x) {
  out <- collector()
  for (i in seq_along(x)) {
    if (i > 1) {
      out$add("")
    }
    out$add(render_para(x[[i]]))
  }
  out$get()
}


render_tparam <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    sprintf(
      "template <%s>",
      paste(vcapply(x, identity), collapse = ", "))
  }
}


render_enumvalue <- function(x) {
  ## TODO: add a div around this, probably around many other things...
  if (!is.null(x$detail)) {
    stop("write support")
  }
  ## TODO: might not want enumerator here?
  c(md_code_block(sprintf("enumerator %s", x$name)),
    render_paras(x$brief))
}


render_detail <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  out <- collector()
  for (el in x) {
    out$add(render_para(el), TRUE)
  }

  drop_trailing_whitespace(out$get())
}


## Might distinguish this later by adding a div/class
render_brief <- render_detail


render_section <- function(x) {
  info <- switch(
    x$kind,
    "public-type" = list("Public types", render_typedef),
    "public-func" = list("Public methods", render_function),
    "public-static-func" = list("Public static methods", render_function),
    "public-attrib" = list("Public fields", render_field),
    "public-static-attrib" = list("Public static fields", render_field))
  label <- info[[1]]
  render <- info[[2]]

  out <- collector()
  out$add(md_bold(label))
  for (el in x$value) {
    out$add(render(el), TRUE)
  }
  out$get()
}


render_parameters <- function(x) {
  out <- collector()
  name <- switch(x$kind,
                 "templateparam" = "Template parameters",
                 "param" = "Parameters",
                 stop("not handled"))
  out$add(md_bold(name), TRUE)
  for (p in x$value) {
    p_name <- md_bold(paste(md_code(p$name), collapse = ", "))
    p_str <- render_paras(p$description)
    stopifnot(length(p_str) > 0)
    p_str[[1]] <- sprintf("* %s: %s", p_name, p_str[[1]])
    ## TODO: check this renders correctly, probably only do the
    ## nonempty cases?
    ##
    ## TODO: Perhaps combine into some sort of generic list building
    ## machinery?
    if (length(p_str) > 1) {
      ## https://www.markdownguide.org/basic-syntax/#unordered-lists
      p_str[-1] <- paste0("    ", p_str[-1])
    }
    out$add(p_str, TRUE)
  }
  out$get()
}


render_simplesect <- function(x) {
  out <- collector()
  title <- switch(x$kind,
                  "return" = "Returns",
                  stop("FIXME"))
  ## TODO: this is same logical level as above
  out$add(md_bold(title), TRUE)
  out$add(render_paras(x$value), TRUE)
  out$get()
}


render_itemizedlist <- function(x) {
  out <- collector()
  for (el in x$items) {
    value <- render_paras(el)
    value[[1]] <- paste0("* ", value[[1]])
    if (length(value) > 1L) {
      ## https://www.markdownguide.org/basic-syntax/#unordered-lists
      value[-1] <- paste0("    ", value[-1])
    }
    out$add(value)
  }
  out$get()
}


render_field <- function(x) {
  out <- collector()
  out$add(md_code_block(sprintf("%s %s%s", x$value, x$name, x$args %||% "")))
  out$add(render_brief(x$brief), TRUE)
  out$add(render_detail(x$detail), TRUE)
  out$get()
}


md_code_block <- function(code) {
  c("```c++", code, "```")
}


md_bold <- function(string) {
  sprintf("**%s**", string)
}


md_code <- function(string) {
  sprintf("`%s`", string)
}


drop_trailing_whitespace <- function(x) {
  i <- grepl("^\\s*$", x)
  if (all(i)) {
    return(NULL)
  }
  if (!any(i)) {
    return(x)
  }
  x[seq_along(i) <= max(which(!i))]
}
