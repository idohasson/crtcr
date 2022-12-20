prepare_vec <- function(.nt, ..., .aa=translate(.nt)) {

  df_list(..., .unpack = TRUE, .name_repair = "minimal")

  prepare_vec(.nt = 1:3) %>% new_data_frame(names=c("clone", "clonotype", "repertoire", "group"))


}
