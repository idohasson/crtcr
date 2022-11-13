
#' CR-class of a clonotype by its share-level among groups.
#'
#' @describeIn
#'
#' given clonotype frequencies in each group, calculate its convergent
#' recombination public classifications based on various conditions.
#'
#' @param shared numerical vector
#' @param min_shared minimum numerical value
#' @param min_public minimum sum from all frequencies having a minimal value to be considered as public clonotype
#' @param max_exclusive maximal number of public clonotypes groups to be considered as public exclusive. o.w. inclusive.
#'
#' @return character object of "private" / "inclusive" / "exclusive" / NA_character_
#' @export
#'
#' @examples
#'
#' cr_class(c(1,0,1,0)) # public inclusive
#'
#' cr_class(c(2,0,0,0)) # public exclusive
#'
#' cr_class(c(1,0,0,0)) # private
#'
cr_class <- function(shared, min_shared=1, min_public=2, max_exclusive=1) {

  is_shared <- shared >= min_shared
  # returns TRUE if the vector of frequencies has at least one non-missing value.
  any_shared <- any(is_shared, na.rm = TRUE)
  # returns TRUE if the sum of frequencies is greater than or equal to the minimum public frequency.
  public <- sum(shared[is_shared], na.rm = TRUE) >= min_public
  # returns TRUE if the number of groups having minimal frequency value is less than or equal to the maximum exclusive frequency.
  exclusive <- public & sum(is_shared, na.rm = TRUE) <= max_exclusive
  # return the appropriate class label based on the values in the previous lines.
  case_when(
    exclusive ~ "exclusive",
    public ~ "inclusive",
    any_shared ~ "private",
    TRUE ~ NA_character_
  ) # returns NA if none of the above conditions are met.
}

cr_class_df <- function(rep_groups, clonotype, rid, gid, ..., min_freq=1, public_min=2, exclusive_max=1) {

  share_level_df(rep_groups, {{clonotype}}, {{rid}}, {{gid}}, ...) %>%

    group_by({{clonotype}}, ..., .add = FALSE) %>%

    summarise(cr_class=cr_class(share, min_freq, public_min, exclusive_max), .groups = "drop")
}


#' Title x
#'
#' @param rep_groups  x
#' @param clonotype  x
#' @param rid x
#' @param gid x
#' @param ... x
#' @param min_freq  x
#' @param public_min  x
#' @param exclusive_max x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' groupList2DF(l) %>% dplyr::rename(group="gid") %>% dplyr::mutate(pid=gl(2,1,nrow(.)))  %>% cr_class_df(clonotype, rid, group, pid)
#'
#'
cr_class_tbl <- function(rep_groups, clonotype, rid, gid, ..., min_freq=1, public_min=2, exclusive_max=1) {

  cr_class_df(rep_groups, clonotype, rid, gid, ..., min_freq, public_min, exclusive_max) %>%

    table()

}














