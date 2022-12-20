cr_norm <- function(nt, index, id) {

  i <- vec_group_loc(translate(nt))

  x1 <- vec_chop(x, i$loc)

  x2 <- vec_chop(df$id, i$loc)

  mapply(cr_subset, x1, x2)

}
