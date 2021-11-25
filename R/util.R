`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}
