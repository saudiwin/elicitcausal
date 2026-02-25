# Shared test helpers

#' Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b
