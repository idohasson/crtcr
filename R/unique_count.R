unique_count <- function(x) {
  UseMethod("unique_count")
}

value_count.character <- function(x, ...) {
  x <- rlang::squash_chr(dots_splice(x, ...))
  vapply(x, vec_unique_count, integer(1), ...)
}


value_count.list <- function(x, ...) {
  x_list <- dots_splice(x, ...)
  lapply(rlang::squash(x_list), value_count, ...)
}


value_count.data.frame <- function(x, ..., i=1) {
  value_count(x[i], ...)
}

unique_count.logical <- function(x, ...) {
  sum(x)
}


unique_count.numeric <- function(x, ...) {
  unique_count(x > 0)
}

unique_count.array <- function(x, ..., i=1) {
  apply(x, i, unique_count)
}

unique_mean <- function(x, ...) {
  unique_count(x, ...) / vec_size(x)
}



