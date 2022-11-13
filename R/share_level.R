
#' Compute the share level of clonotypes in a group
#'
#' @param data data frame
#' @param rid repertoire identification column name
#' @param clonotype_var clonotype sequence column name
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' require(dplyr)
#'
#' rand_group() %>%
#' groupList2DF() %>%
#' group_by(gid) %>%
#' share_level(rid, clonotype)
#'
share_level <- function(data, rid, clonotype_var) {

  group_by(data, {{clonotype_var}}, .add = TRUE) %>%

  summarise(across({{rid}}, n_distinct, .names = "share"))

}



#' #' Count table for number of each clonotype found in a repertoire in a specific gorup
#' #'
#' #' @param df data frame with clonotype column, group ID coliumn and repertoire ID column
#' #' @param clonotype clonotype column name
#' #' @param group_id Group ID column name
#' #' @param rep_id Repertoire ID column name
#' #' @param ... any additional column name
#' #'
#' #' @return table
#' #'
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' df_lists <- replicate(4, rand_group(), FALSE)
#' #' rand_df <- group_join(df_lists)
#' #' share_table(rand_df, "clonotype", "group", "rep_id")
#' #'
#' share_df <- function(df, group_id, rep_id, clonotype) {
#'
#'   group_by(df, group_id) %>%
#'
#'     share_level(rep_id, clonotype) %>%
#'
#'     tidyr::spread(group, share_level)
#'
#'   # tidyr::gather(group, share_level, -clonotype, na.rm = TRUE)
#'
#'   # unique_by <- c(clonotype, group_id, rep_id, ...)
#'   #
#'   # df[unique_by] %>% distinct() %>%
#'   #
#'   #   select(unique_by[1:2]) %>% table
#' }



#' add_share_level <- function(data, rid, clonotype_var) {
#'
#'   data %>% group_by({{clonotype_var}}, .add = TRUE) %>%
#'
#'     mutate(across({{rid}},  n_distinct, .names = "share"))
#'
#' }
#'
#'
#'
#' #' #' Title
#' #' #'
#' #' #' @description x
#' #' #'
#' #' #' @param group_list x
#' #' #' @param share_method x
#' #' #'
#' #' #' @return x
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #'
#' #' #' gl <- replicate(4, rand_group())
#' #' #'
#' #' #' share_table(gl, share_method = "CRlevel")
#' #' #'
#' #' #'
#' #' share_table <- function(group_list, share_method="unique") {
#' #'
#' #'   group_list %>%
#' #'
#' #'   map_dfr(share.level, method=share_method, .id = "gid") %>%
#' #'
#' #'   spread(gid, share)
#' #'
#' #' }
#'
#'
#' #' #' Title
#' #' #'
#' #' #' @param group
#' #' #' @param method
#' #' #'
#' #' #' @return
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #'
#' #' #' g <- rand_group()
#' #' #'
#' #' #' share_level(g)
#' #' #'
#' #' #'
#' #' share_level <- function(df, method="unique") {
#' #'
#' #'   method <- arg_match0(method, c("unique", "relative", "CRlevel", "avgCRlevel"))
#' #'
#' #'   method_list <- list(
#' #'
#' #'     "unique" = as_function(~ n_distinct(.y)), # unique clonal sequences
#' #'
#' #'     "relative" = as_function(~ n_distinct(.y) / n_distinct(.x)), # average unique clonal sequences
#' #'
#' #'     "CRlevel" = as_function(~ n_distinct(.x, .y)), # total CR-level
#' #'
#' #'     "avgCRlevel" = as_function(~ n_distinct(.x, .y) / n_distinct(.x)) # average CR-level
#' #'   )
#' #'
#' #'   f <- method_list[[method]]
#' #'
#' #'   share_level(df, )
#' #'   # f(df)
#' #'
#' #'   # group_by(df, clonotype) %>%
#' #'   #
#' #'   # summarise(share=f(rid, clone))
#' #'
#' #' }
#'
#'
#'
#'
#'
#' # rand_df %>%
#'   # group_by(group, rep_id) %>%
#'   # cr_level(clone, clonotype) %>%
#'   # group_by(group) %>%
#'   # share_level(rep_id, clonotype) %>%
#'   # tidyr::spread(group, share_level) %>%
#'   # tidyr::gather(group, share_level, -clonotype, na.rm = TRUE)
#'
#'   # add_share_level(rep_id, clonotype) %>%
#'   # group_by(group, clonotype) %>%
#'   # summarise(avgCR_level=mean(CR_level) / share_level, .groups = "drop_last")
#'   # mutate(avg=CR_level/share_level)
#'   # filter(clonotype=="A") %>% as.data.frame()
#'
#'
#'
#'
