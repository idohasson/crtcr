# cr_prop(x)
# cr_prop(l)
cr_prop <- function(nt, val=cr_number(nt)) {

  val / vctrs::vec_size(nt)

}
