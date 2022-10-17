
cr_level_per_sample <- function(dtl) {
  acast(dtl,
        aaSeqCDR3 ~ sample,
        value.var = "nSeqCDR3",
        fun.aggregate = n_distinct)
}

sharing_level_per_gruop <- function(dtl) {
  acast(dtl,
        aaSeqCDR3 ~ group,
        value.var = "sample",
        fun.aggregate = n_distinct)
}


cr_lvl <- function(clones) {
  fct_count(factor(clones))
}

cr_lvl.list.vec <- function(clones) {
  map_dfr(clones, cr_lvl, .id = "sample")
}

cr_table <- function(clones) {
  cr_lvl(clones) %>%
    xtabs(formula = n ~ f + sample)
}



