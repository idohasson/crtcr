
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

#' Title x
#'
#' @param rep_groups  x
#' @param clonotype x
#' @param rid x
#' @param gid x
#' @param ... x
#' @param min_shared x
#' @param min_public x
#' @param exclusive_max c
#'
#' @return x
#' @export
#'
#' @examples
#'
#' p <- replicate(4, rand_group(), FALSE)
#'
#' df <- groupList2DF(p)
#'
#' df <- cbind(df, pid=ifelse(as.numeric(df$gid)%%2, "human", "mouse"))
#'
#' head(df)
#'
#' cr_df <- cr_class_df(df, clonotype, rid, gid, pid)
#' head(cr_df)
#'
cr_class_df <- function(rep_groups, clonotype, rid, gid, ..., min_shared=1, min_public=2, exclusive_max=1) {

  share_level_df(rep_groups, {{clonotype}}, {{rid}}, {{gid}}, ...) %>%

    group_by({{clonotype}}, ..., .add = FALSE) %>%

    summarise(cr_class=cr_class(share, min_shared, min_public, exclusive_max), .groups = "drop")
}


#' Title x
#'
#' @param rep_groups  x
#' @param clonotype  x
#' @param rid x
#' @param gid x
#' @param ... x
#' @param min_shared x
#' @param min_public x
#' @param exclusive_max x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' p <- replicate(4, rand_group(), FALSE)
#' df <- groupList2DF(p)
#'
#' cr_tbl <- cr_class_tbl(df, clonotype, rid, gid)
#' head(cr_tbl)
#'
#' df <- cbind(df, pid=ifelse(as.numeric(df$gid)%%2, "human", "mouse"))
#'
#' cr_tbl <- cr_class_tbl(df, clonotype, rid, gid, pid)
#' head(cr_tbl)
#'
cr_class_tbl <- function(rep_groups, clonotype, rid, gid, ..., min_shared=1, min_public=2, exclusive_max=1) {

  cr_class_df({{rep_groups}}, {{clonotype}}, {{rid}}, {{gid}}, ..., min_shared = min_shared, min_public = min_public, exclusive_max = exclusive_max) %>%

    table()

}














