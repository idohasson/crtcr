library("purrr")
library("forcats")
library("tidyr")





map_dfc(group_vec, state_count) %>%
  proportions %>%
  as.data.frame()

d <- data.frame(aa=sample(LETTERS[1:3], 20, replace = TRUE), s=gl(2, 1, 20))
d
t <- table(d)
t
p <- proportions(t)
p
as.data.frame(p)


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


  # reduce(group1, union) %>% intersect(public(group1, group2, %>% %>% %>% %>% <- <- <- <-  group3, group4))
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
