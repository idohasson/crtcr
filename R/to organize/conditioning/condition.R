condition <- function(..., .condition) {

  condition_call <- substitute(.condition)

  dfl <- df_list(..., .name_repair = "minimal")

  val <- new_data_frame(dfl)

  i_cond <- eval(condition_call, val)

  i_cond

}
