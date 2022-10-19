library("purrr")
library("forcats")
library("tidyr")




# public <- function(dfl, gl, clonotype, compare_by) {
#
#   get_gruops(dfl, gl, clonotype, compare_by) %>%
#     as.data.table %>%
#     sharing_level_per_gruop %>%
#     apply(1, state)
#
#   # public(dfl, list(1:3, 4:6), "aaSeqCDR3", "nSeqCDR3") %>% as.list %>% as_tibble
# }


# public_seq.df <- function(df) {
#
#   is_singltone <- function(vec) sum(as.logical(vec))==1
#
#   t <- df %>%
#     with(table(sample, group, aaSeqCDR3)) %>%
#     as.data.frame %>%
#     xtabs(formula = Freq ~ aaSeqCDR3+group) %>%
#     addmargins(margin = 2,
#                FUN = list(list(sample_freq=sum,
#                                gruop_freq=is_singltone)))
#
#   pri_seq <- t[t[,"sample_freq"]==1,] %>% rownames
#   # pub_seq <- t[t[,"sample_freq"]!=1,] %>% rownames
#   ex_seq <- t[t[,"sample_freq"]!=1 & t[,"gruop_freq"]==1,] %>% rownames
#   in_seq <- t[t[,"sample_freq"]!=1 & !t[,"gruop_freq"]==1,] %>% rownames
#
#   list(private=pri_seq, public=list(exclusive=ex_seq, inclusive=in_seq))
# }


public_table <- function(seq_list1, seq_list2, ...) {

  seq_list <- list(seq_list1, seq_list2, ...)
  vec_list <- modify_depth(seq_list, .depth = 2, unique)
  vec_group <- map(vec_list, reduce, union)

  sample_sharing <- table(unlist(vec_list))
  gruop_sharing <- table(simplify(vec_group))


  state_count <- function(seq_vec) {

    s_table <- sample_sharing[seq_vec] == 1
    g_table <- gruop_sharing[seq_vec] != 1

    private_n <- sum(s_table)

    inclusive_n <- sum(!s_table & g_table)

    exclusive_n <- sum(!s_table & !g_table)

    c(private_n, inclusive_n, exclusive_n)

  }

  map_dfc(group_vec, state_count)
}



# map_dfc(group_vec, state_count) %>%
#   proportions %>%
#   as.data.frame()



public_seq <- function(vec_list) {

  sharing_level <- table(simplify(vec_list))

  names(keep(sharing_level, ~ . != 1))

}

exclusive_seq <- function(vec_list) {

  group_sharing <- table(simplify(vec_list))

  names(discard(group_sharing, ~ . != 1))

}


function() df$aaSeqCDR3 %>% split(f = gl(20, 1, length = length(df$aaSeqCDR3))) %>% map(unique) %>% unlist %>% table %>% discard(~ . == 1)


# public sequences from multiple list of vectors
public <- function(seq_list1, seq_list2, ...) {

  vec_list <- c(seq_list1, seq_list2, ...)

  unique_clonotypes <- map(vec_list, unique)

  public_seq(unique_clonotypes)

}

exclusive <- function(seq_list1, seq_list2, ...) {

  ps <- public(seq_list1, seq_list2, ...)

  list(seq_list1, seq_list2, ...) %>%
    map(reduce, union) %>%
    map(intersect, y=ps) %>%
    simplify %>%
    table %>%
    keep(. == 1) %>%
    names


  # reduce(group1, union) %>% intersect(public(group1, group2, group3, group4))
  #
  #
  #
  # grouped_lists <- list(seq_list1, seq_list2, ...) %>%
  #   modify_depth(grouped_lists, .depth = 2, unique)
  #
  #
  #
  # groups_public <- map(grouped_lists, public_seq)
  # groups_public
  # exclusive_seq(groups_public)

}



# public sequences from multiple list of dataframess' AA columns


# public sequences from dataframe with AA column and group column
