new_clone <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = "clone")
}

clone <- function(x = character()) {
  x <- vec_cast(x, character())
  new_clone(x)
}

is_clone <- function(x) {
  inherits(x, "clone")
}

as_clone <- function(x, ...) {
  UseMethod("as_clone")
}

as_clone.default <- function(x, ...) {
  vec_cast(x, new_clone())
}

as_clone.character <- function(x) {
  value <- as.character(toupper(x))
  new_clone(value)
}



nt_vec <- c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
            'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG')


as_clone(nt_vec)
