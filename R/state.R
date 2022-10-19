library(data.table)
library(dplyr)



state_count <- function(tbl, group_list) {

  group_share <- map_dfc(group_list, function(i) rowSums(tbl[,i]>1))
  # group_share
  rowSums(group_share != 0)

  # group_list %>%
  #   lapply(select, .data=df) %>%
  #   map_dfc(function(aa) rowSums(aa>1)) %>%
  #   apply(1, function(g) rowSums(g>1))

  # gl <- list(A=fields(df)[1:3], B=fields(df)[4:6])
  # df <- as.data.frame(df)
  # state_count(as.data.frame(cr.tbl), gl)
}

state <- function(aa) {
  # v <- sum(aa == 1)
  case_when(
    aa == 1 ~ "private",
    aa > 1 ~ "inclusive",
    TRUE ~ "exclusive" # aa == 0
  )
}



aa_count.data.frame <- function(df, aa_col, group_col) {
  df %>%
    pivot_wider(
      names_from = group_col,
      values_from = group_col,
      values_fn = n_distinct,
      values_fill = 0
    )
  # df %>%
  #   group_by_at(aa_col) %>%
  #   summarise(share=state.data.frame(group_col))
}












