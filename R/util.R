`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}
