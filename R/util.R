`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


collector <- function(init = character(0)) {
  env <- new.env(parent = emptyenv())
  env$res <- init
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


drop_whitespace <- function(x) {
  gsub(" +", "", x)
}
