
#' Title
#'
#' @description x
#'
#' @param group_list x
#' @param share_method x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' gl <- replicate(4, rand_group())
#'
#' share_table(gl, share_method = "CRlevel")
#'
#'
share_table <- function(group_list, share_method="unique") {

  group_list %>%

  map_dfr(share.level, method=share_method, .id = "gid") %>%

  spread(gid, share)

}


#' Title
#'
#' @description x
#'
#' @param clone_list x
#' @param method x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' rep_list <- rand_group()
#'
#' share.level(rep_list, method="relative")
#'
share.level <- function(clone_list, method="unique") {

  method <- arg_match0(method, c("unique", "relative", "CRlevel", "avgCRlevel"))

  method_list <- list(

    "unique" = as_function(~ n_distinct(.y)), # unique clonal sequences

    "relative" = as_function(~ n_distinct(.y) / n_distinct(.x)), # average unique clonal sequences

    "CRlevel" = as_function(~ n_distinct(.x, .y)), # total CR-level

    "avgCRlevel" = as_function(~ n_distinct(.x, .y) / n_distinct(.x)) # average CR-level
  )

  f <- method_list[[method]]

  replist2DF(clone_list) %>%

  group_by(clonotype) %>%

  summarise(share=f(rid, clone))

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
#' share_df <- function(df, clonotype, group_id, rep_id, ...) {
#'
#'   unique_by <- c(clonotype, group_id, rep_id, ...)
#'
#'   df[unique_by] %>% distinct() %>%
#'
#'     select(unique_by[1:2]) %>% table
#' }
