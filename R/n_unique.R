# insight::n_unique
n_unique <- function(x, ...) {
  UseMethod("n_unique")
}

n_unique.default <- function(x, ..., na.rm = FALSE) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  vec_unique_count(x)
}

n_unique.data.frame <- function(x, ..., na.rm = FALSE) {
  sapply(x, n_unique, na.rm = na.rm)
}

n_unique.list <- function(x, ..., na.rm = FALSE) {
  lapply(x, n_unique, na.rm = na.rm)
}

n_unique.cr <- function(cr, ..., na.rm = FALSE) {
  rapply(cr, n_unique, classes = "character", deflt = NA_integer_, how = "unlist")
}

has_single_value <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  !is.null(x) && isTRUE(all(x == x[1]))
}
