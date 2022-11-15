
#' CR level of clonotyoes in a repertoire
#'
#' @param .data repertoire data frame
#' @param .clone clonal sequence variable
#' @param ... additional variables to group by
#'
#' @return dataframe with `CRlevel` column
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#' group_id=gl(2,4,8,labels = c('cancer','control')),
#' sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#' nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#'      'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
#' aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
#' )
#' cr_level(df, nt)
#' cr_level(df, nt, sample_id)
#'
#' require('dplyr')
#'
#' group_by(df, group_id) %>% cr_level(nt)
#' group_by(df, group_id) %>% cr_level(nt, sample_id)
#'
cr_level <- function(.data, .clone, ...) {

  .data %>%

    group_by(.cr=translate({{.clone}}), ..., .add = TRUE) %>%

    add_tally(wt = n_distinct({{.clone}}), name = 'CRlevel') %>%

    ungroup('.cr') %>% select(!'.cr')
}



#' Share level of clonotypes among repertoires
#'
#' @param .data repertoire data frame
#' @param .clonotype clonotype sequence variable
#' @param .rep_id repertoire identification column
#' @param ... additional variables to group by
#'
#' @return data frame with 'share' column
#' @export
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
#' share_level(df, aa, sample_id)
#'
#' require('dplyr')
#'
#' group_by(df, group_id) %>% share_level(aa, sample_id)
#'
share_level <- function(.data, .clonotype, .rep_id, ...) {

  .data %>%

    group_by({{.clonotype}}, ..., .add = TRUE) %>%

    add_tally(wt = n_distinct({{.rep_id}}), name = "share")
}



#' Public clonotype in repertoire collection
#'
#' @param .data repertoire data frame
#' @param .clonotype clonotype sequence variable
#' @param .rep_id repertoire identification column
#' @param ... additional variables to group by
#'
#' @return data frame with 'public' column
#' @export
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
#' @export
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


