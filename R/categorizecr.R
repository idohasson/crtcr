# table(cr_categorize(cr_list$inv.freq))
cr_categorize <- function(lvl) {
  categorize(lvl, split = "quantile", n_groups = 3)
}
