# cr_number(x)
cr_number <- function(nt) {

  if (rlang::is_list(nt))

    nt <- rlang::squash_chr(nt)

  vec_unique_count(nt)

}



# cr_number(NT[2:5],NT[5:7])
cr_number <- function(...) {

  nt_list <- dots_splice(...)

  vapply(nt_list, vec_unique_count, integer(1))
  # do.call(vec_unique_count, nt_list)

}
