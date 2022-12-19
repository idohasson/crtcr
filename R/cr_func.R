sapply(rand_clone_list(), cr_prop)
cr_prop <- function(nt) {

  vec_unique_count(nt) / vec_size(nt)

}

with(rand_rep_df(), cr_subset(nt, aa))
cr_subset <- function(nt, aa) {




}
