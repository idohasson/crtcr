unique_count <- function(x, ...) {
  UseMethod("unique_count")
}

unique_count.character <- function(x, ...) {
  vec_unique_count(rlang::squash_chr(list(x, ...)))
}


unique_count.list <- function(x, ...) {
  vapply(squash_if(list(x, ...), is_list), unique_count, integer(1))
  # unique_count()
}


unique_count.data.frame <- function(x, ...) {
  vec_unique_count(dplyr::bind_rows(x, ...))
}

unique_count.logical <- function(x, ...) {
  sum(x,..., na.rm = TRUE)
}


unique_count.numeric <- function(x, ...) {
  unique_count(c(x, ...) > 0)
}

unique_count.array <- function(x, ..., i=1) {
  apply(x, i, unique_count)
}

unique_mean <- function(x, ...) {
  sum(unique_count(list(x, ...))) / vec_size(x)
}
