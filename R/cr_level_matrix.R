#' claculate the CR level in each clonotype in every sample
#'
#' @description Here, we define CR level as the number of different NT sequences encoding a specific AA sequence.
#'
#' @param clonotype_list
#'
#' @return
#' @export
#'
#' @examples
#' # Generate a random list of letters (as the clonotype sequences) from the letters vector.
#' l <- replicate(6, sample(LETTERS, sample(13:18, 1, replace = TRUE)), simplify = FALSE)
#' cr_level_table(l) # number of vectors a letter found in
#'
#'
cr_level_matrix <- function(clonotype_list) {

  fl <- lapply(clonotype_list, as_factor)

  fl <- setNames(fl, seq_along(fl))

  df <- map_dfr(fl, vec_count, .id = "sample")

  df <- rename(df, clonotype = "key")

  tapply(df$count, df[c("clonotype", "sample")], sum, default = 0)

}
