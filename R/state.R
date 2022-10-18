library(data.table)

state <- function(aa) {
  # v <- sum(aa == 1)
  case_when(
    aa == 1 ~ "private",
    aa > 1 ~ "inclusive",
    TRUE ~ "exclusive" # aa == 0
  )
}

state.data.frame <- function(group) {

  n <- n_distinct(group)

  case_when(
    n == 1 ~ "private",
    n > 1 ~ "inclusive",
    TRUE ~ "exclusive" # aa == 0
  )
}

singletone_count <- function(aa_n) {
  sum(aa_n==1)
}

# aa_count.data.frame <- function(df, aa_col, group_col) {
#   df %>%
#     pivot_wider(
#       names_from = group_col,
#       values_from = group_col,
#       values_fn = n_distinct,
#       values_fill = 0
#     )
#   # df %>%
#   #   group_by_at(aa_col) %>%
#   #   summarise(share=state.data.frame(group_col))
# }












