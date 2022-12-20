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
