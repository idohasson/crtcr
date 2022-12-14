is_cr <- function(clone, ...) {
  UseMethod("is_cr")
}


is_cr.default <- function(clone, ...) {

  is_cr(as.character(clone))

}

is_cr.character <- function(clone, cr_seq) {

  if (is_missing(cr_seq)) {

    cr_seq <- translate(clone[1])

    clone <- clone[-1]

  }


  for (nt in clone)

    if (cr_seq != translate(nt))

      return(FALSE)

  return(TRUE)

}



