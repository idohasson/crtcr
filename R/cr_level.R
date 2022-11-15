#' Convergent recombination level (CR-level) of clonal sequences
#'
#' @description
#' calculate unique number of nucleotide sequences encoding a specific amino
#' acid sequence. This value associated with TCR repertoire clonal sequences
#' and termed as "CR-level" (convergent recombination level).
#'
#' @param ... nucleotide sequences
#' @param always_named name single value. Default `TRUE`.
#'
#' @return CR-level integer vector (named by the clonotype)
#' @export
#' @examples
#' cr_level("ATGTTT", "ATCTTC", "ATGTTC", "AAA")
#'
cr_level_vec <- function(clonal_seq, always_named=FALSE) {

  clonal_sequence <- c(...)

  clonal_sequence %<>%

    split(translate(.)) %>%

    sapply(n_distinct)

  if (always_named | !testScalar(clonal_sequence))

    return(clonal_sequence)

  else

    return (unname(clonal_sequence))

}


#' Convergent Recombination level for data frame's clonal sequence column
#'
#' @description
#' calculate unique number of nucleotide sequences encoding a specific amino
#' acid sequence. This value associated with TCR repertoire clonal sequences
#' and termed as "CR-level" (convergent recombination level).
#'
#'
#' @param df data frame
#' @param ... Column names to group the clones by as distinct repertoires
#' @param clone_var Column name of CDR3's nucleotide sequence
#' @param clonotype_var (optional) Column name of CDR3's amino acid sequence
#'
#' @return data frame
#' @export
#'
#' @examples
#' #'
#' df <- data.frame(
#'   group_id=gl(2,4,8,labels = c('cancer','control')),
#'   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#'   nt=c('CGGGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#'        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTCAAG'),
#'   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
#' )
#' #'
#' cr_level_df(df, clone_var=nt, clonotype_var=aa)
#'
#' cr_level_df(df, clone_var=nt)
#'
#' cr_level_df(df, group_id, sample_id, clone_var=nt, clonotype_var=aa)
#'
cr_level_df <- function(df, ..., clone_var, clonotype_var) {

  if (missingArg(clonotype_var)) {
    df <- mutate(df, clonotype=translate({{clone_var}}))
    clonotype_var <- as.name('clonotype')
  }

  group_by(df, {{clonotype_var}}, ..., .add = FALSE) %>%

    summarise(across({{clone_var}}, n_distinct, .names = "CR_level"), .groups = "drop")
}




# df <- data.frame(
#   group_id=gl(2,4,8,labels = c('cancer','control')),
#   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#   nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
#   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
# )
# cr_level2(df, nt)
# cr_level2(df, nt, sample_id)
# group_by(df, group_id) %>% cr_level2(nt)
# group_by(df, group_id) %>% cr_level2(nt, sample_id)
cr_level2 <- function(.data, .clone, ...) {

  .data %>%

    group_by(.cr=translate({{.clone}}), ..., .add = TRUE) %>%

    add_tally(wt = n_distinct({{.clone}}), name = "CRlevel") %>%

    ungroup(.cr) %>% select(-.cr)
}

# df <- data.frame(
#   group_id=gl(2,4,8,labels = c('cancer','control')),
#   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#   nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
#   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
# )
# TODO use as example
#      add grouping
# averaged_cr_level(df, nt, sample_id)
# group_by(df, group_id) %>% averaged_cr_level(nt, sample_id)
averaged_cr_level <- function(.data, .clone, .id, ...) {

  .data %>%

  group_by(.cr=translate({{.clone}}),
           .add = TRUE) %>%

  add_tally(wt = n_distinct({{.clone}}) / n_distinct({{.id}}),
            name = "avaregedCRlevel") %>%

  ungroup(.cr) %>% select(-.cr)

}



# share_level2(df, aa, sample_id)
# group_by(df, group_id) %>% share_level2(aa, sample_id)
share_level2 <- function(.data, .clonotype, .id, ...) {

  .data %>%

    group_by({{.clonotype}},
             .add = TRUE) %>%

    add_tally(wt = n_distinct({{.id}}),
              name = "share")
}

public_cr <- function(.data, .clonotype, .rid, .gid) {

  .data %>%

    group_by({{.clonotype}}, .add = TRUE) %>%

    mutate(
      cr=case_when(
        n_distinct({{.rid}}) == 1 ~ 'private',
        TRUE ~ 'public'
      )
    )
}
# public_cr(df, aa, sample_id, group_id)

exclusive_cr <- function(.data, .clonotype, .rid, .gid) {

  .data %>%

    group_by({{.clonotype}}, .add = TRUE) %>%
    # summarise(n_distinct({{.gid}}), n_distinct({{.rid}}))

    mutate(
      cr=case_when(
        n_distinct({{.gid}}) == 1 & n_distinct({{.rid}}) != 1 ~ 'exclusive',
        n_distinct({{.gid}}) != 1 & n_distinct({{.rid}}) != 1 ~ 'inclusive',
        TRUE ~ 'private'
      )
    )
}

# exclusive_cr(df, aa, sample_id, group_id)


