library(tidyverse)
library(rlang)
library(vctrs)


prepare_vec <- function(.nt, ..., .aa=translate(.nt)) {

  df_list(..., .unpack = TRUE, .name_repair = "minimal")

  prepare_vec(.nt = 1:3) %>% new_data_frame(names=c("clone", "clonotype", "repertoire", "group"))


}

# prepare_list(replicate(2, replicate(3, rand_nt_list(v = sample(4, 1)), F), F))
prepare_list <- function(data_list) {

  flat_list <- squash_if(data_list, is_list)

  data <- map_dfr(flat_list, as_tibble_col, column_name = "clone", .id = "id")

  prepare_df(data, .nt = clone, id)

}

# prepare_df(id, aa, .df=rand_rep_df(), .aa=aa, .nt=nt, .rid = id, id2)
prepare_df <- function(.df, ..., .nt, .aa, .rid) {

  # TODO:
  #   if (isFALSE(group_pos>0)) length(group_pos) # or NA
  #   eval_select(where(is.factor),.data)
  #   eval_select(where(is.character),.data)
  #   eval_select(where(is.numeric),.data)

  vars <- get_pos(.df,
          .clone = {{ .nt }},
          .clonotype = {{ .aa }},
          .repertoire = {{ .rid }},
          ...)

  vars_pos <- lapply(vars, unname)

  arg_select <- append(list(.data = .df), vars_pos)

  # TODO list of with selected data frame and varible names
  do.call(select, arg_select)

}

# get_pos(rand_rep_df(), nt, aa)
get_pos <- function(.data, .clone, .clonotype, .repertoire, ...) {

  # TODO: check cols

  clone_pos <- eval_select(enquo(.clone), .data)

  clonotype_pos <- eval_select(enquo(.clonotype), .data)

  repertoire_pos <- eval_select(enquo(.repertoire), .data)

  group_pos <- eval_select(expr(c(...)), .data)

  list(clone=clone_pos,
       clonotype=clonotype_pos, # TODO VDJ
       repertoire=repertoire_pos,
       group=group_pos) # TODO more than one group

  # TODO fix seq_along
  # sapply(pos_list, function(x) {
  #   ifelse(is_named(x) & length(x)>0, names(x), seq_along(x))
  # })
}

