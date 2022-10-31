# g1 <- rand_group(rep_type="df_list", n_sample=7, seq_n = 100, seq_len=5)
# g2 <- rand_group(rep_type="df_list", n_sample=12, seq_n = 100, seq_len=5)
#
# sub_g1_i <- list(1:3, 4:7)
# sub_g2_i <- list(1:5, 6:12)
#
# cr1 <- cr_list(g1, "aa", sub_g1_i)
# cr2 <- cr_list(g2, "aa", sub_g2_i)
#
# names_g <- c("Cancer", "Control")
#
# populations <- setNames(list(cr1, cr2), names_g)
# cr_overlap(populations)
cr_overlap <- function(subpopulations, func=intersect_percentage) {

  cross(subpopulations) %>%

    lapply(setNames, names(formals(func))) %>%

    invoke_map(.f = func) %>%

    array(dim = c(3,3), dimnames = lapply(subpopulations, names))
}

# aa_vec1 <- rand_rep_vec("aa", 10000, 5)
# aa_vec2 <- rand_rep_vec("aa", 10000, 5)
#
# intersect_percentage(aa_vec1, aa_vec2)
## # https://github.com/GreiffLab/immuneREF/blob/master/R/repertoire_overlap.R
intersect_percentage <- function(x, y){
  c(length(intersect(x,y)))/min(c(length(x), length(y)))
}






