
# cr_subset(nt, 4:7)
# cr_subset(x, translate(x))
cr_subset <- function(nt, id=seq_along(nt), func=cr_prop) {

  nt_chopped <- cr_loc(nt, id)

  func(nt_chopped)

}
# vec_as_location(-1, 3, dimnames(tbl))
# cr_loc(c("E", "R", "K"), translate(x))
cr_loc <- function(nt, id) {

  i <- vec_group_loc(id)

  vec_chop(nt, i$loc)

}
