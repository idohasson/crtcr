

join_groups <- function(g, aa="aaSeqCDR3", nt="nSeqCDR3") map_dfr(g, distinct_at, c(aa, nt), .id = "sample")

tbl <- distinct_at(df, c("group", "sample", "nSeqCDR3", "aaSeqCDR3")) %>%
  select(-nSeqCDR3) %>%
  sample_n(100) %>%
  with(table(aaSeqCDR3, group))

tbl
library("tibble")
rowSums(tbl, dims = 2) %>% add_column()


vec <- list(cancer=vec1, control=vec2)

vec_aa <- unlist(vec, recursive = FALSE)

cr_level <- table(unlist(vec_aa, use.names = FALSE))

is.private <- cr_level==1


vec_list1 <- map(group1, pull, "aaSeqCDR3")
vec_list2 <- map(group2, pull, "aaSeqCDR3")

df <- list.files("data/Alpha/", full.names = TRUE)[-7] %>%
  map_dfr(read_tsv, col_types = mixcr_column_type, id = "sample") %>%
  mutate(sample=file_path_sans_ext(basename(sample))) %>%
  distinct_at(c("sample", "nSeqCDR3", "aaSeqCDR3"))

df %>%  split(~aaSeqCDR3) %>%

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



library("data.table")

clone_table <- function(df) {
  df %>% distinct(nSeqCDR3, aaSeqCDR3, sample, group) %>%
    with(table(aaSeqCDR3, sample, group)) %>%
    addmargins

}

cr_level <- function(df) {
  ct <- clone_table(df)
  x[,-ncol(x),"Sum"]
}

sharing_level <- function(df) {

  ct <- clone_table(df)

  ct[,"Sum",-3]
}

is.public <- function(df) {

  ct <- clone_table(df)

  p <- ct[,-ncol(ct),"Sum"] %>%
    apply(2, as.logical) %>%
    rowSums()!=1
  ct[p,-ncol(ct),"Sum"]
}
