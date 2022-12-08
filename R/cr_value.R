library(magrittr)
library(rlang)
library(vctrs)
library(tibble)


cr_func <- function(clone,
                    .seq_func=NULL, # each sequence
                    .clone_func=NULL, # each unique sequence group
                    .clonotipic_func=NULL, # each group
                    ...)
  {


  if (!is_function(.seq_func))

    .seq_func <- length

  seq_list <- lapply(clone, .seq_func)


  group_clone_list <- split(seq_list, clone)

  if (!is_function(.clone_func))

    group_values <- lapply(group_clone_list, length)

  else

    group_values <- lapply(group_clone_list, do.call, what=.clone_func)

  if (!is_function(.clonotipic_func))

    results_values <- length(group_values)

  else

    results_values <- do.call(.clonotipic_func, group_values)

  results_values


}

# clonal_freq(function(s) s %>% strsplit(split = "") %>% unlist %>% table %>% length)
clonal_freq <- function(clonal_seq, .func=length, ...) {
  vapply(clonal_seq, .func, numeric(1), ..., USE.NAMES = FALSE)
}


clonal_value <- function(clone, .func=length, ..., .seq_value=clonal_freq) {

  freq <- .seq_value(clone)

  grouped_freq <- vec_split(freq, clone)

  vapply(grouped_freq$val, .func, numeric(1), ..., USE.NAMES = FALSE)

}

cr_value <- function(clone, .clonotipc_func=length, ..., .freq_func=clonal_value) {

  clonal_freq <- .freq_func(clone)

  .clonotipc_func(clonal_freq, ...)

}

my_freq_func <- function(s) clonal_value(s, sum, .seq_value = nchar)
cr_value(y, mean, .freq_func = my_freq_func)




clonal_freq(y, table)
cr_value(y, mean)

clonal_freq(y) %>% split(y)

y %>%

  vapply(length, numeric(1), USE.NAMES = FALSE) %>%
  split(y) %>% clonal_value(sum)


vapply(length, numeric(1), USE.NAMES = FALSE) %>%
  length

vec_split()
