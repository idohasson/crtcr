# cr_list(rand_nt_vec(100,1))
cr_list <- function(nt, aa=NULL, named=FALSE) {

  is_cr <- !duplicated(nt)

  nt <- nt[is_cr]

  if (is_null(aa)) {

    aa <- translate(nt)

  } else {

    aa <- aa[is_cr]

  }

  cr_split <- vec_split(nt, aa)

  if (isTRUE(named)) {

    return(tibble::deframe(cr_split))

  } else {

    return(cr_split$val)

  }

}
