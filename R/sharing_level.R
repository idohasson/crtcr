
#' count clonotype-sharing among individuals
#'
#' @description for a given list of amino acid sequences vectors of each individual/sample count number of samples each clonotype is presented
#'
#' @param clonotype_list list of character vectors
#'
#' @return data frame - number of vectors each sequence presented in
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

share_prop <- function(clone_list, ...) {
  # list(clone_list, ...)
  fl <- lapply(l, as_factor)
  names(fl) <- letters[seq_along(fl)]

  map_dfr(fl, fct_count, .id = "S") %>%
    xtabs(formula = n ~ f + S)

  f <- fct_c(!!!fl)

  fct_count(f) %>%
    xtabs(formula = n ~ f)

  function(clones) {
    fct_count(factor(clones))
  }

  function(clones) {
    cr_lvl(clones) %>%
      xtabs(formula = n ~ f + sample)
  }

  cr_table(clone_list, ...) %>%
    reshape2::melt(id = "type") %>%
    xtabs(formula = value ~ variable + type)
  # %>%
  #   proportions("type")
}



sharing_level_per_gruop <- function(dtl) {

  acast(dtl,
        aaSeqCDR3 ~ group,
        value.var = "sample",
        fun.aggregate = n_distinct)

}
