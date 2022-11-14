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

  # if(n_distinct(clonotype_sequence) == 1) {
  #   return(n_distinct(clonal_sequence))
  # }

  results <- tapply(clonal_sequence, clonotype_sequence, n_distinct)

  if (length(results)==1)
    return(unname(results))
  else
    return(results)


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
cr_level_df <- function(df, ..., clonal_var, clonotype_var) {

  if (missingArg(clonotype_var)) {
    df <- dplyr::mutate(df, clonotype=translate({{clonal_var}}))
    clonotype_var <- as.name(dplyr::last(names(df)))
  }

  group_by(df, {{clonotype_var}}, ..., .add = FALSE) %>%

  summarise(across({{clonal_var}}, n_distinct, .names = "CR_level"), .groups = "drop")

}



#' CR-level throughout all sub-groups matrix
#'
#' @param df data frame
#' @param clone_var clonal seq variable name
#' @param clonotype_var clonotype seq variable name
#' @param ... repertoire ID and any other group ID column
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#'
#' df <- data.frame(nt=c("ATG","TTC","TAT","TTT","ATG","ATG"),
#'                  aa=c("M","F","Y","F","M","M"),
#'                  rep_id = gl(3,1, 6),
#'                  group_id = gl(2, 3, labels = c("cancer", "control")))
#'
#' cr_level_tbl(df, clone_var = nt, clonotype_var = aa, group_id, rep_id)
#'
cr_level_tbl <- function(df, ..., clonal_var, clonotype_var) {

  cr_level_df(df, ..., clonal_var={{clonal_var}}, clonotype_var={{clonotype_var}}) %>%

  xtabs(formula = CR_level ~ .)

}


cr_level_list <- function(...) {

    map_dfr(.x = rlang::dots_splice(...),
            .f = . %>% data.frame(clone=., clonotype=translate(.)),
            .id = "rid") %>%

    distinct %>% with(table(clonotype, rid))

}


