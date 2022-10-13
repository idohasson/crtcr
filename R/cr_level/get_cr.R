
cr_level.data.frame <- function(df) {
  df %>%
    distinct(sample, nSeqCDR3, aaSeqCDR3) %>%
    with(table(aaSeqCDR3, sample))
}

avarege_cr_level.data.frame <- function(df) {
  df %>%
    cr_level.data.frame %>%
    apply(MARGIN = 2, mean)
}


# group_field="sample"
# nt_field="nSeqCDR3"
# aa_field="aaSeqCDR3"
# tbl=as_tibble(df)

# tbl %>%
#   distinct_at(.vars = c(group_field, nt_field, aa_field)) %>%
#   select(aa_field, group_field) %>%
#   table
