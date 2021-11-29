`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


collector <- function() {
  env <- new.env(parent = emptyenv())
  env$res <- character(0)
  add <- function(x, new_block = FALSE) {
    add_break <- new_block && length(env$res) > 0 && length(x) > 0
    env$res <- c(env$res,
                 if (add_break) "" else NULL,
                 x)
  }

  list(add = add,
       empty = function() length(env$res) == 0,
       get = function() env$res)
}


collector_list <- function() {
  env <- new.env(parent = emptyenv())
  env$res <- list()
  append <- function(x) {
    env$res <- c(env$res, x)
  }
  list(add = function(x) append(list(x)),
       append = append,
       get = function() env$res)
}


xml_is_missing <- function(x) {
  inherits(x, "xml_missing")
}


msg <- function(text, quiet) {
  if (!quiet) {
    message(text)
  }
}
