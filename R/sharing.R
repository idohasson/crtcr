#' #################### share-level table ####################
#' # clonotype sharing between individuals #
#'
#' # Previous studies have shown that the extent of sharing and the
#' # clonotypic frequency of TCRb sequences are significantly corre
#' # lated with their production efficiencies in simulations of a random
#' # recombination process because of the phenomenon of convergent
#' # recombination. https://doi.org/10.1073/pnas.1319389111
#'
#'
#'
#' #' Title
#' #'
#' #' @param ... data frames or vectors
#' #'
#' #' @return table
#' #'
#' #' @importFrom dplyr distinct
#' #' @importFrom magrittr %>% %$%
#' #'
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' # generate random
#' #' df_lists <- replicate(4, rand_group(), FALSE)
#' #' rand_df <- group_join(df_lists)
#' #' names(rand_df)
#' #'
#' sharing <- function(...) { # DF
#'   # a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the columns of the flat contingency table.
#'
#'   # Clonotype
#'   df <- group_join(...)
#'
#'
#'
#'   # unique_clontype_df <- distinct_at(df, all_of())
#'
#'   # sharing()
#'   clonotype_share_level <- unique_clontype_df %>%
#'
#'       select_at(all_of(vars(c("clonotype", "group")))) %>%
#'
#'       table
#'
#'   clonotype_share_level
#'
#'   # Clone
#'   unique_clone_df <- distinct_at(df, all_of(vars(c("group", "rep_id", "clone"))), .keep_all = TRUE)
#'
#'   clonal_seq_share_level <- unique_clone_df %>%
#'
#'     select_at(all_of(vars(c("clone", "group")))) %>%
#'
#'     table
#'
#'   clonal_seq_share_level
#'
#'   # distinct(clonotype, group) %$%
#'   # table(clonotype, group)
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
