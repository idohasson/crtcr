#' #' Share level of repertoire group
#' #'
#' #' @description for a given list of amino acid sequences vectors of each individual/sample count number of samples each clonotype is presented
#' #'
#' #' @param clonotype_list list of character vectors
#' #'
#' #' @return data frame - number of vectors each sequence presented in
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' # Generate a random list of letters (as the clonotype sequences).
#' #'
#' #' aa <- replicate(4, rand_rep_vec("aa", seq_n = 100), simplify = FALSE)
#' #'
#' #' share_level(l)
#' #'
#' share_level <- function(clonotype_list) {
#'   # make sure each vector has unique sequences
#'   clonotype_list <- lapply(clonotype_list, unique)
#'   # flatten the list of lists into a single list
#'   clonotype <- unlist(clonotype_list)
#'   # generate a frequency table of the clonotype sample collection
#'   clonotype_n <- table(clonotype)
#'   # convert the frequency table into a data frame
#'   as.data.frame(clonotype_n, responseName = c("shared"))
#' }
#'
#'
#' #' #' Share level of repertoire group
#' #' #'
#' #' #' @param rep_list data frame list
#' #' #' @param clonotype_col name
#' #' #' @param clonotype_col name
#' #' #'
#' #' #' @return
#' #' #'
#' #' #' @importFrom purrr every
#' #' #' @importFrom utils hasName
#' #' #'
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #'
#' #' #' group <- rand_group()
#' #' #'
#' #' #' shared_clonotype(group, clonotype_col = "aa")
#' #' #'
#' #' shared_clonotype <- function(rep_list, clonotype_col) {
#' #'
#' #'   stopifnot(every(rep_list, hasName, name=clonotype_col))
#' #'
#' #'   rep_list <- map(rep_list, pull, clonotype_col)
#' #'
#' #'   share_level(rep_list)
#' #'
#' #' }
#' #'
#' #' shared_clone <- function(rep_list, clone_col) {
#' #'
#' #'   stopifnot(every(rep_list, hasName, name=clone_col))
#' #'
#' #'   rep_list <- map(rep_list, pull, clone_col)
#' #'
#' #'   share_level(rep_list)
#' #'
#' #' }
#' #'
#' #'
#' #' #' Share level of multiple group
#' #' #'
#' #' #' @param rep_list
#' #' #' @param clonotype_col
#' #' #' @param population_i
#' #' #'
#' #' #' @return
#' #' #'
#' #' #' @importFrom purrr every
#' #' #'
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' shared_population_clonotype <- function(rep_list, clonotype_col, population_i) {
#' #'
#' #'   stopifnot(every(rep_list, is.data.frame))
#' #'
#' #'   get_clonotype_aa <- function(indices)
#' #'
#' #'     lapply(rep_list[indices], get_aa, clonotype_col)
#' #'
#' #'   rep_list <- lapply(population_i, get_clonotype_aa)
#' #'
#' #'   share_table(rep_list)
#' #' }
#' #'
#' #'
#' #' # shared_population_clone
#' #'
#' #'
#' #' #' Title
#' #' #'
#' #' #' @param rep_list
#' #' #' @param clone_col
#' #' #' @param population_i
#' #' #'
#' #' #' @return
#' #' #'
#' #' #' @importFrom purrr every
#' #' #'
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' shared_population_clone <- function(rep_list, clone_col, population_i) {
#' #'
#' #'   stopifnot(every(rep_list, is.data.frame))
#' #'
#' #'   get_clone_nt <- function(indices)
#' #'     lapply(rep_list[indices], get_nt, clone_col)
#' #'
#' #'   rep_list <- lapply(population_i, get_clone_nt)
#' #'
#' #'   share_table(rep_list)
#' #' }
#' #'
#' #'
#' #' #' Title
#' #' #'
#' #' #' @param populations_list
#' #' #'
#' #' #' @return
#' #' #'
#' #' #' @importFrom purrr every purrr map_depth flatten_chr
#' #' #' @importFrom tidyr spread
#' #' #' @importFrom tibble spread column_to_rownames
#' #' #'
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' share_table <- function(populations_list) {
#' #'
#' #'   stopifnot(every(populations_list, is.list))
#' #'
#' #'   if (vec_depth(populations_list) == 3)
#' #'     populations_list <- map_depth(populations_list, 2, unique)
#' #'
#' #'   map(populations_list, flatten_chr) %>%
#' #'     # counts the number of times each clonotype appears in a sample vector
#' #'     map_dfr(vec_count, .id = "group") %>%
#' #'     # takes the group column and the count column and spreads the count
#' #'     # column into multiple columns, one for each unique value in the group
#' #'     # column. The resulting data frame has one row for each unique value
#' #'     # in the group column and one column for each unique value in the
#' #'     # count column.
#' #'     spread(group, count, 0) %>%  # convert the resulting data frame
#' #'     # from long format to wide forma
#' #'     column_to_rownames("key")   # convert the group column into row names
#' #' }
#' #'
#' #'
#' #'
