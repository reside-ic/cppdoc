do_render_function <- function(x) {
  out <- collector()

  decl <- paste0(x$name, x$args)
  if (length(x$value) > 0) {
    decl <- paste(paste(trimws(x$value), collapse = " "), decl)
  }

  out$add(md_code_block(c(do_render_tparam(x$tparam), decl)))

  out$add(do_render_brief(x$brief), TRUE)
  out$add(do_render_detail(x$detail), TRUE)

  out$get()
}


do_render_define <- function(x) {
  if (!is.null(x$brief)) {
    stop("Write rendering for brief")
  }
  if (!is.null(x$detail)) {
    stop("Write rendering for detail")
  }

  md_code_block(sprintf("#define %s %s", x$name, x$value))
}


do_render_enum <- function(x) {
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
    out$add(do_render_enumvalue(el))
  }

  out$get()
}


do_render_typedef <- function(x) {
  out <- collector()
  out$add(md_code_block(c(
    do_render_tparam(x$tparam),
    x$definition_short)))

  out$add(do_render_brief(x$brief), TRUE)
  out$add(do_render_detail(x$detail), TRUE)

  out$get()
}


do_render_para <- function(x) {
  out <- collector()
  if (x$type != "para") {
    stop("check this")
  }
  for (el in x$value) {
    if (!out$empty()) {
      out$add("")
    }
    if (el$type == "text") {
      out$add(el$value)
    } else if (el$type == "parameters") {
      name <- switch(el$kind,
                     "templateparam" = "Template parameters",
                     "param" = "Parameters",
                     stop("not handled"))
      out$add(md_bold(name))
      out$add("") # only if length(el$value) > 0
      for (p in el$value) {
        p_name <- md_bold(paste(md_code(p$name), collapse = ", "))
        p_str <- do_render_paras(p$description)
        stopifnot(length(p_str) > 0)
        p_str[[1]] <- sprintf("* %s: %s", p_name, p_str[[1]])
        ## TODO: check this renders correctly, probably only do the
        ## nonempty cases?
        if (length(p_str) > 1) {
          p_str[-1] <- paste0("  ", p_str[-1])
        }
        out$add(p_str)
      }
    } else {
      stop("FIXME")
    }
  }
  out$get()
}


do_render_class <- function(x) {
  out <- collector()
  out$add(md_code_block(c(do_render_tparam(x$tparam),
                          paste("class", x$name))))

  out$add(do_render_brief(x$brief), TRUE)
  out$add(do_render_detail(x$detail), TRUE)

  for (s in x$sections) {
    out$add(do_render_section(s), TRUE)
  }

  out$get()
}


do_render_paras <- function(x) {
  out <- collector()
  for (i in seq_along(x)) {
    if (i > 1) {
      out$add("")
    }
    out$add(do_render_para(x[[i]]))
  }
  out$get()
}


do_render_tparam <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    sprintf(
      "template <%s>",
      paste(vcapply(x, identity), collapse = ", "))
  }
}


do_render_enumvalue <- function(x) {
  ## TODO: add a div around this, probably around many other things...
  if (!is.null(x$detail)) {
    stop("write support")
  }
  ## TODO: might not want enumerator here?
  c(md_code_block(sprintf("enumerator %s", x$name)),
    do_render_paras(x$brief))
}


do_render_detail <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  out <- collector()
  for (el in x) {
    out$add(do_render_para(el), TRUE)
  }

  drop_trailing_whitespace(out$get())
}


## Might distinguish this later by adding a div/class
do_render_brief <- do_render_detail


do_render_section <- function(x) {
  info <- switch(
    x$kind,
    "public-type" = list("Public types", do_render_typedef),
    "public-func" = list("Public methods", do_render_function),
    "public-static-func" = list("Public static methods", do_render_function),
    "public-attrib" = list("Public fields", do_render_field),
    "public-static-attrib" = list("Public static fields", do_render_field))
  label <- info[[1]]
  render <- info[[2]]

  out <- collector()
  out$add(md_bold(label))
  for (el in x$value) {
    out$add(render(el), TRUE)
  }
  out$get()
}


do_render_field <- function(x) {
  browser()
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
