do_render_function <- function(x) {
  out <- collector()

  out$add(md_code_block(c(
    do_render_tparam(x$tparam),
    paste(trimws(x$value), collapse = " "),
    paste0(x$name, x$args))))

  if (!is.null(x$brief)) {
    stop("writeme")
  }

  for (el in x$detail) {
    out$add("")
    out$add(do_render_para(el))
  }

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
  if (!is.null(x$brief)) {
    stop("Write rendering for brief")
  }
  if (!is.null(x$detail)) {
    stop("Write rendering for detail")
  }

  out <- collector()
  out$add(md_code_block(c(
    do_render_tparam(x$tparam),
    x$definition)))
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


md_code_block <- function(code) {
  c("```c++", code, "```")
}


md_bold <- function(string) {
  sprintf("**%s**", string)
}


md_code <- function(string) {
  sprintf("`%s`", string)
}
