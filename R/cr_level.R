#' Convergent Recombination level (CR-level)
#'
#' @param clonal_sequence x
#' @param clonotype_sequence x
#'
#' @return data frame
#'
#' @export
#'
#' @examples
#'
#' cr_level(c("ATG", "TTT", "ATC", "TTC", "TTG", "TTA"))
#'
cr_level <- function(clonal_sequence, clonotype_sequence) {

  if (missingArg(clonotype_sequence))

    clonotype_sequence <- translate(clonal_sequence)

  if(n_distinct(clonotype_sequence) == 1) {
    return(n_distinct(clonal_sequence))
  }

  tapply(clonal_sequence, clonotype_sequence, n_distinct)

}


#' Convergent Recombination level (CR-level)
#'
#' @param df c
#' @param clonal_var c
#' @param clonotype_var x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' clone_vec <- c("ATG", "TTT", "ATC", "TTC", "TTG", "TTA")
#'
#' (df <- rep2DF(clone_vec))
#'
#' cr_level_df(df, clone, clonotype)
#'
#' clone_list <- list(c("ATG", "TTT", "ATC"), c("TTC", "TTG", "TTA"))
#'
#' (df2 <- repList2DF(clone_list))
#'
#' cr_level_df(dplyr::group_by(df2, rid))
#'
#'
cr_level_df <- function(df, clonal_var, clonotype_var) {

  if (missingArg(clonotype_var))
    clonotype_var <- as.name(nth(names(df), -1))

  if (missingArg(clonal_var))
    clonal_var <- as.name(nth(names(df), -2))

  group_by(df, {{clonotype_var}}, .add = TRUE) %>%

  summarise(across({{clonal_var}}, cr_level, .names = "CR_level"))

}
