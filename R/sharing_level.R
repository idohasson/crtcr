#' count clonotype-sharing among individuals
#'
#' @description for a given list of amino acid sequences vectors of each individual/sample count number of samples each clonotype is presented
#'
#' @param clonotype_list list of character vectors
#'
#' @return data frame - number of vectors each sequence presented in
#'
#' @export
#'
#' @examples
#' # Generate a random list of letters (as the clonotype sequences) from the letters vector.
#' l <- replicate(6, sample(LETTERS, sample(13:18, 1, replace = TRUE)), simplify = FALSE)
#' share_level(l) # number of vectors a letter found in
#'
share_level <- function(clonotype_list) {
  # make sure each vector has unique sequences
  clonotype_list <- lapply(clonotype_list, unique)
  # flatten the list of lists into a single list
  clonotype <- unlist(clonotype_list)
  # generate a frequency table of the clonotypes
  clonotype_n <- table(clonotype)
  # convert the frequency table into a data frame
  as.data.frame(clonotype_n, responseName = c("sharing"))
}

# barchart(Titanic, scales = list(x = "free"), auto.key = list(title = "Survived"))
