library("purrr")

# prepare
rand_aa <- function(vec_size) do.call(paste0, Map(stri_rand_strings, n=vec_size, length=5, pattern = '[A-C]'))

group1 <- replicate(3, rand_aa(10), simplify = FALSE)
group2 <- replicate(4, rand_aa(10), simplify = FALSE)
group3 <- replicate(5, rand_aa(10), simplify = FALSE)
group4 <- replicate(6, rand_aa(10), simplify = FALSE)

public_table(group1, group2, group3, group4)





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
