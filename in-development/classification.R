#################### CR-class ####################


# CR-logic


#' is clonotype public by sub-groups share-level
#'
#' @param shared numerical vector
#' @param min_shared minimal value to be counted as repertoire having a clonotype
#' @param min_public minimal number of repertoire having a clonotype to be conciser public
#'
#' @return TRUE if its public, FALSE if private, and NA if none of the frequencies are sufficient
#'
#' @examples
#'
#' is_public(c(0,1,0,0)) # FALSE (private clonotype)
#'
#' is_public(c(0,2,0,0)) # FALSE (public clonotype)
#'
#' is_public(c(0,2,0,0), min_public = 3) # FALSE (private clonotype)
#'
#' is_public(c(0,1,2,3), min_public = 5) # TRUE (public clonotype)
#'
#' is_public(c(0,2,0,0), min_shared=3) # NA (value is not sufficient)
#'
#' is_public(c(0,1,2,3), min_shared=3) # TRUE (public clonotype)
#'
#' is_public(c(0,1,2,3), min_shared=3, min_public = 5) # FALSE (private clonotype)
#'




is_private <- sharing_samples == 1
is_public <- sharing_samples > 1
is_exclusive <- is_public & (sharing_groups == 1)
is_inclusive <- is_public & (sharing_groups > 1)


is_public.freq <- function(shared, min_shared=1, min_public=2*min_shared) {

  in_sub_gruop <- shared >= min_shared

  if (!any(in_sub_gruop, na.rm = FALSE)) return(NA)

  sum(shared[in_sub_gruop], na.rm = TRUE) >= min_public
}

is_public.freq <- function(..., min_public=2) {

  freq <- c(...)

  rep_freq <- share_level(freq)

  if (rep_freq == 0)

    return(NA)

  else if (min_public > 1)

    return(rep_freq >= min_public)

  else

    return(rep_freq / length(freq) >= min_public)
}

is_public.freq <- function(..., min_public=2) {

  in_rep <- c(...)>0

  in_group <- sum(in_rep, na.rm = TRUE)

  if (in_rep == 0)
    return(NA)

  if (min_public <= 1)
    in_group <- in_group / length(in_rep)

  in_group >= min_public

}

is_shared_public <- function(..., min_public=2) {

  shared <- sum(..., na.rm = TRUE) >= min_public

  if (shared==0)
    return(NA)

  shared >= min_public

}

# super public

is_super_public <- function(...) {

  is_public(..., min_public=.5)

}

# exclusive

#' logical check if a frequencies vector is considered public-exclusive with optional modifications
#'
#' @param shared numerical vector
#' @param max_exclusive upper limit to number of groups the could be considered exclusive
#'
#' @return TRUE for exclusive, FALSE for inclusive, and NA if non of the groups are public
#'
#' @examples
#'
#' is_exclusive(c(0,0,0,3)) # TRUE (exclusive public)
#'



is_exclusive <- function(gruop_freq, max_exclusive=1) {

  gruop_freq <- share_level(gruop_freq)

  if (gruop_freq == 0)

    return(NA)

  else if (max_exclusive >= 1)

    return(gruop_freq <= max_exclusive)

  else

    return(gruop_freq / length(gruop_freq) <= max_exclusive)

}



#' Public clonotype in repertoire collection
#'
#' @param .data repertoire data frame
#' @param .clonotype clonotype sequence variable
#' @param .rep_id repertoire identification column
#' @param ... additional variables to group by
#'
#' @return data frame with 'public' column
#'
#' @examples
#'
#' df <- data.frame(
#'   group_id=gl(2,4,8,labels = c('cancer','control')),
#'   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#'   nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#'        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
#'   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
#' )
#'
#' public_class(df, aa, sample_id, group_id)
#'
public_class <- function(.data, .clonotype, .rep_id, ...) {

  .data %>%

    group_by({{.clonotype}}, ..., .add = TRUE) %>%

    mutate(public=case_when(

      n_distinct({{.rep_id}}) > 1 ~ 'public',

      TRUE ~ 'private'

    ))
}

#' Public clonotype sub-class of repertoire groups
#'
#' @param .data repertoire data frame
#' @param .clonotype clonotype sequence variable
#' @param .rep_id repertoire identification column
#' @param .group_id group identification column
#' @param ... additional variables to group by
#'
#' @return data frame with 'cr' column
#'
#' @examples
#'
#' df <- data.frame(
#'   group_id=gl(2,4,8,labels = c('cancer','control')),
#'   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#'   nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#'        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
#'   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
#' )
#'
#' cr_class(df, aa, sample_id, group_id)
#'
cr_class <- function(.data, .clonotype, .rep_id, .group_id, ...) {

  .data %>%

    group_by({{.clonotype}}, ..., .add = TRUE) %>%

    mutate(cr=case_when(

      n_distinct({{.rep_id}}) != 1 & n_distinct({{.group_id}}) == 1 ~ 'exclusive',

      n_distinct({{.rep_id}}) != 1 & n_distinct({{.group_id}}) != 1 ~ 'inclusive', # TODO remove first condition

      TRUE ~ NA_character_

    ))
}

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
#'
#' @examples
#'
#' cr_class(c(1,0,1,0)) # public inclusive
#'
#' cr_class(c(2,0,0,0)) # public exclusive
#'
#' cr_class(c(1,0,0,0)) # private
#'
public_cr_class <- function(share_tbl, min_shared=1, min_public=2, max_exclusive=1) {

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

cr_class.tbl <- function(..., public_min=1, exclusive_min=1) { # vector

  cr_index <- function(n) {
    #     numeric vector of the unique number of samples
    #       having a specific clonotype in every group
    (max(n, na.rm = TRUE) > 1) + (sum(n != 0, na.rm = TRUE) > 1)
    #   can't be private             multiple shared samples
    #   public clonotype               inclusive clonotype
  }

  apply(share_tbl, 1, cr_index) %>%
    # private = 0 | exclusive = 1 | inclusive = 2
    {case_when(. == 0 ~ "private",
               . == 1 ~ "exclusive",
               . == 2 ~ "inclusive")}
}

factor_cr <- function(group_count) {

  compute_type <- function(tbl)
    # public clonotype    inclusive clonotype
    # can't be private    multiple shared samples
    (rowSums(tbl) > 1) + (rowSums(tbl != 0) > 1)
  # private = 0 | exclusive = 1 | inclusive = 2

  factor(compute_type(group_count),
         levels = c(0, 1, 2),
         labels = c("private", "exclusive", "inclusive"))
}

