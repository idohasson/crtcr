# # https://github.com/GreiffLab/immuneREF/blob/master/R/repertoire_overlap.R

# aa_vec1 <- rand_rep_vec("aa", 10000, 5)
# aa_vec2 <- rand_rep_vec("aa", 10000, 5)
#
# intersect_percentage(aa_vec1, aa_vec2)
#
intersect_percentage <- function(x, y){
  c(length(intersect(x,y)))/min(c(length(x), length(y)))
}

# g1 <- rand_group(rep_type="df_list", n_sample=7, seq_n = 100, seq_len=5)
# sub_g1_i <- list(1:3, 4:7)
# g1_aa_cr <- cr_list(g1, "aa", sub_g1_i)
#
# g2 <- rand_group(rep_type="df_list", n_sample=12, seq_n = 100, seq_len=5)
# sub_g2_i <- list(1:5, 6:12)
# g2_aa_cr <- cr_list(g2, "aa", sub_g2_i)
#
# pairwise_cr(g1_aa_cr, g2_aa_cr, func)
#
pairwise_cr <- function(a, b, f=intersect_percentage) {

  sub_names <- c("private", "exclusive", "inclusive")

  dims <- list(paste0(deparse(substitute(a)), sep = "$", sub_names),
               paste0(deparse(substitute(b)), sep = "$", sub_names))
  cross2(a, b) %>%

    invoke_map(.f = f) %>%

    array(dim = c(3,3), dimnames = dims)

}


# g1 <- rand_group(rep_type="df_list", n_sample=7, seq_n = 100, seq_len=5)
# g2 <- rand_group(rep_type="df_list", n_sample=12, seq_n = 100, seq_len=5)
#
# sub_g1_i <- list(1:3, 4:7)
# sub_g2_i <- list(1:5, 6:12)
#
# cr_group_ovelap(g1, g2, sub_g1_i, sub_g2_i)
cr_group_ovelap <- function(g1, g2, sub_g1_i, sub_g2_i, clonotype=TRUE, clone=TRUE, func=intersect_percentage) {

  g1_aa_cr <- cr_list(g1, "aa", sub_g1_i)
  g2_aa_cr <- cr_list(g2, "aa", sub_g2_i)

  g1_nt_cr <- cr_list(g1, "nt", sub_g1_i)
  g2_nt_cr <- cr_list(g2, "nt", sub_g2_i)

  list(clonotype = pairwise_cr(g1_aa_cr, g2_aa_cr, func),
       clone = pairwise_cr(g1_nt_cr, g2_nt_cr, func))

}







