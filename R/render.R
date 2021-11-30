render_function <- function(x, control) {
  force(control)
  out <- collector()

  ## TODO: we should render a better decl here, by rendering
  ## parameters.
  decl <- sprintf("%s(%s)", x$name, render_params(x$param, control))

  ## NOTE: this is zero for constructors, typically.
  ##
  ## TODO: need to have the ability to ignore the attributes that we
  ## just spend time supporting...
  ##
  ## NOTE: I suspect that `const double&` might come out as a vector
  ## here too?
  if (length(x$value) > 0) {
    decl <- paste(render_linked_text(x$value, control), decl)
  }

  out$add(html_code_block(c(render_tparam(x$tparam, control), decl)))

  out$add(render_brief(x$brief, control), TRUE)
  out$add(render_detail(x$detail, control), TRUE)

  out$get()
}


render_define <- function(x, control) {
  force(control)
  message("fix rendering of value in render_define")
  browser()

  out <- collector()
  out$add(md_code_block(sprintf("#define %s %s", x$name, x$value)))
  out$add(render_brief(x$brief, control), TRUE)
  out$add(render_detail(x$detail, control), TRUE)
  out$get()
}


render_enum <- function(x) {
  force(control)
  ## TODO: I *think* but am not sure that 'enum class' vs 'enum'
  ## changes the "strong" field here.
  type <- if (x$strong == "yes") "enum class" else "enum"
  out <- collector()
  out$add(md_code_block(sprintf("%s %s", type, x$name)))

  out$add(render_brief(x$brief, control), TRUE)
  out$add(render_detail(x$detail, control), TRUE)

  if (length(x$enumvalues) > 0) {
    out$add("")
    out$add(md_bold("Values"))
  }
  for (el in x$enumvalues) {
    out$add("")
    out$add(render_enumvalue(el, control))
  }

  out$get()
}


render_typedef <- function(x, control) {
  force(control)
  out <- collector()

  tparam <- render_tparam(x$tparam, control)

  ## TODO: presence of @typedef in the docstrings might break this logic
  ##
  ## TODO: function pointers are hard here, and will require
  ## additional work to render correctly.
  name <- x$name
  type <- render_linked_text(x$type, control)
  if (grepl("^using", x$definition)) {
    definition <- sprintf("using %s = %s", name, type)
  } else {
    definition <- sprintf("typedef %s %s", type, name)
  }

  out$add(html_code_block(c(tparam, definition)))
  out$add(render_brief(x$brief, control), TRUE)
  out$add(render_detail(x$detail, control), TRUE)

  out$get()
}


## TODO: Looks like we're inconsistent throughout as to if we pass
## $value or not, would be good to check that.
render_para <- function(x, control) {
  force(control)
  out <- collector()
  for (el in x$value) {
    value <- switch(el$type,
                    "text" = el$value,
                    "computeroutput" = render_computeroutput(el, control),
                    "link" = render_link(el, control),
                    "parameters" = render_parameters(el, control),
                    "simplesect" = render_simplesect(el, control),
                    "itemizedlist" = render_itemizedlist(el, control),
                    stop(sprintf("Unsupported type '%s'", el$type)))
    out$add(value, TRUE)
  }
  out$get()
}


render_computeroutput <- function(x, control) {
  force(control)
  ## We only get here if we have complex markup that we can't preserve
  ## properly.  It would be nice to otherwise try and push a link out
  ## as far as possible so that if we have
  ## <code><bold><ref>...</ref></bold><code>

  ## we can lift this out to order as ref/code/bold, but not really
  ## sure how best to do that; would need to be within the parse I
  ## think, on the simple markup tags
  sprintf("<code>%s</code>", render_para(x, control))
}


render_link_target <- function(target, control) {
  if (is.null(control$link)) {
    return(NULL)
  }
  i <- match(target, control$link$from)
  if (is.na(i)) {
    return(NULL)
  }
  control$link$to[[i]]
}


render_link <- function(x, control, html = FALSE) {
  force(control)
  target <- render_link_target(x$target, control)
  browser()
  if (is.null(target)) {
    x$value
  } else if (html) {
    sprintf('<a href="%s">%s</a>', target, x$value)
  } else {
    sprintf("[%s](%s)", x$value, target)
  }
}


render_linked_text <- function(x, control) {
  force(control)
  if (x$type == "linked_text")  {
    paste(vcapply(x$value, render_linked_text_element, control),
          collapse = "")
  } else {
    render_linked_text_element(x, control)
  }
}


render_linked_text_element <- function(x, control) {
  force(control)
  switch(x$type,
         text = x$value,
         link = render_link(x, control, html = TRUE),
         stop("Unexpected linked_list element"))
}


render_class <- function(x, control) {
  force(control)
  out <- collector()
  out$add(md_code_block(c(render_tparam(x$tparam, control),
                          paste("class", x$name))))

  out$add(render_brief(x$brief, control), TRUE)
  out$add(render_detail(x$detail, control), TRUE)

  for (s in x$sections) {
    out$add(render_section(s, control), TRUE)
  }

  out$get()
}


render_paras <- function(x, control) {
  force(control)
  out <- collector()
  for (i in seq_along(x)) {
    out$add(render_para(x[[i]], control), TRUE)
  }
  out$get()
}


render_tparam <- function(x, control) {
  force(control)
  if (is.null(x)) {
    NULL
  } else {
    message("Fix render_tparam")
    browser()
    sprintf(
      "template <%s>",
      paste(vcapply(x, identity), collapse = ", "))
  }
}


render_enumvalue <- function(x, control) {
  force(control)
  ## TODO: add a div around this, probably around many other things...
  ## TODO: might not want enumerator here?
  c(md_code_block(sprintf("enumerator %s", x$name)),
    render_paras(x$brief, control),
    render_paras(x$detail, control))
}


render_detail <- function(x, control) {
  force(control)
  if (is.null(x)) {
    return(NULL)
  }
  out <- collector()
  for (el in x) {
    out$add(render_para(el, control), TRUE)
  }

  drop_trailing_whitespace(out$get())
}


## Might distinguish this later by adding a div/class
render_brief <- render_detail


render_section <- function(x, control) {
  force(control)
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
    out$add(render(el, control), TRUE)
  }
  out$get()
}


render_params <- function(x, control) {
  force(control)
  args <- vcapply(x, function(el)
    sprintf("%s %s", render_linked_text(el$type, control), el$name))
  paste(args, collapse = ", ")
}


## This is the parameters in the description, not the declaration
## (render_params/render_tparams)
render_parameters <- function(x, control) {
  force(control)
  out <- collector()
  name <- switch(x$kind,
                 "templateparam" = "Template parameters",
                 "param" = "Parameters",
                 stop("not handled"))
  out$add(md_bold(name), TRUE)
  for (p in x$value) {
    p_name <- md_bold(paste(md_code(p$name), collapse = ", "))
    p_str <- render_paras(p$description, control)
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


render_simplesect <- function(x, control) {
  force(control)
  out <- collector()
  title <- switch(x$kind,
                  "return" = "Returns",
                  stop("FIXME"))
  ## TODO: this is same logical level as above
  out$add(md_bold(title), TRUE)
  out$add(render_paras(x$value, control), TRUE)
  out$get()
}


render_itemizedlist <- function(x, control) {
  force(control)
  out <- collector()
  for (el in x$items) {
    value <- render_paras(el, control)
    value[[1]] <- paste0("* ", value[[1]])
    if (length(value) > 1L) {
      ## https://www.markdownguide.org/basic-syntax/#unordered-lists
      value[-1] <- paste0("    ", value[-1])
    }
    out$add(value)
  }
  out$get()
}


render_field <- function(x, control) {
  force(control)
  message("Fix value in render_field")
  browser()
  out <- collector()
  out$add(md_code_block(sprintf("%s %s%s", x$value, x$name, x$args %||% "")))
  out$add(render_brief(x$brief, control), TRUE)
  out$add(render_detail(x$detail, control), TRUE)
  out$get()
}


## Small utilities here, probably will move file
md_code_block <- function(code) {
  c("```c++", code, "```")
}


html_code_block <- function(code) {
  c("<pre>", code, "</pre>")
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
