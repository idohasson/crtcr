


# rep_list <- list(human=list(cancer=rand_group(),
#                             control=rand_group()),
#                  mouse=list(cancer=rand_group(),
#                             control=rand_group()))



# group2DF <- function(rep_list) {
#   map_dfr(rep_list, replist2DF, .id = "gid")
# }
# pop_df <- map_dfr(pop_list, group2DF, .id = "pid")

# pop_df %>%
#
#   sample_n(10) %>%
#
#   group_by(clonotype, pid, gid, rid) %>%
#
#   with(table(clonotype,  pid))









# tbl_list %>% lapply(. %>% select(-clonotype) %>% cr_class)

#' func <- intersect_percentage
#'
#'
#' tbl <- group_list %>%
#'
#'   unlist(recursive = FALSE) %>%
#'
#'   share_table
#'
#' cr_list <- map(names(group_list), . %>%
#'                  dplyr::starts_with(vars = names(tbl)) %>%
#'                  select(.data = tbl) %>% cr_class %>%
#'                  split(x = tbl$clonotype))
#'
#' purrr::cross(cr_list) %>%
#'
#'   lapply(setNames, names(formals(func))) %>%
#'
#'   purrr::invoke_map(.f = func) %>%
#'
#'   array(dim = c(3,3), dimnames = lapply(cr_list, names))
#'
#' #' #' Title
#' #' #'
#' #' #' @param subpopulations list
#' #' #' @param func finction
#' #' #'
#' #' #' @return matrix
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #'
#' #' #' g1 <- rand_group(n_sample=7, seq_n = 100, seq_l=5)
#' #' #' g2 <- rand_group(n_sample=12, seq_n = 100, seq_l=5)
#' #' #'
#' #' #' sub_g1_i <- list(1:3, 4:7)
#' #' #' sub_g2_i <- list(1:5, 6:12)
#' #' #'
#' #' #' cr1 <- cr_list(g1, "aa", sub_g1_i)
#' #' #' cr2 <- cr_list(g2, "aa", sub_g2_i)
#' #' #'
#' #' #' names_g <- c("Cancer", "Control")
#' #' #' populations <- setNames(list(cr1, cr2), names_g)
#' #' #'
#' #' #' cr_overlap(populations)
#' #' #'
#' #' cr_overlap <- function(subpopulations, func=intersect_percentage) { # TODO: overlap as number of intersect. not precentile
#' #'
#' #'   cross(subpopulations) %>%
#' #'
#' #'     lapply(setNames, names(formals(func))) %>%
#' #'
#' #'     invoke_map(.f = func) %>%
#' #'
#' #'     array(dim = c(3,3), dimnames = lapply(subpopulations, names))
#' #' }
#' #'
#' #'
#' #'
#' #'
#' #'
#' #' # aa_vec1 <- rand_rep_vec("aa", 10000, 5)
#' #' # aa_vec2 <- rand_rep_vec("aa", 10000, 5)
#' #' #
#' #' # intersect_percentage(aa_vec1, aa_vec2)
#' #' # cr_simmilarity
#' intersect_percentage <- function(x, y) { # overlap coefficient
#'   length(intersect(x, y)) / min(length(x), length(y))
#' }
#' #'
#' #' jaccard_index <- function(x, y){
#' #'   length(intersect(x, y)) / length(union(x, y))
#' #' }
#' #'
#' #' # TODO: morisita
#' #'
#' #'
#' #'
