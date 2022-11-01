
#' Title
#'
#' @param n_codons
#'
#' @return
#' @export
#'
#' @examples
rand_nt <- function(n_codons) {

  coding_codon <- function() {

    codon <- sample(c("A", "G", "T", "C"), 3, rep = TRUE) %>%

      paste(collapse = "")

    ifelse(codon %in% c("TGA", "TAA", "TAG"), coding_codon(), codon)

  }

  replicate(n_codons, coding_codon()) %>%

    paste(collapse = "")
}

#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
#' @examples
rand_aa <- function(n) {

  AA_list <- c("A","C","D","E","F",
               "G","H","I","K","L",
               "M","N","P","Q","R",
               "S","T","V","W","Y")

  sample(AA_list, n, rep=TRUE) %>%

    paste(collapse = "")
}

#' Title
#'
#' @param vec_len
#' @param seq_len
#' @param type
#'
#' @return
#' @export
#'
#' @examples
rand_rep_vec <- function(seq_type, seq_n = 10, seq_len=13) {

  seq_type <- try(rlang::arg_match(seq_type, c("aa", "nt")))

  rand_f <- ifelse (seq_type == "aa", rand_aa, rand_nt)

  if (length(seq_len)==1) {

    seq_len <- rpois(seq_n, lambda = seq_len)

    seq_len[seq_len==0] <- 1
  }

  sapply(seq_len, rand_f)
}

#' Title
#'
#' @param df_size
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rand_rep_df <- function(...) {

  clone_vec <- rand_rep_vec(seq_type = "nt", ...)

  cbind.data.frame(nt = clone_vec, aa = translate(clone_vec))

}

#' Title
#'
#' @param n_sample
#' @param ...
#' @param rep_type
#'
#' @return
#'
#' @examples
rand_group <- function(n_sample=5, seq_n = rpois(n_sample, 1E3), seq_l=3) {
  if (length(seq_n) > 1)
    lapply(seq_n, function(n) rand_rep_df(seq_n = n, seq_len=seq_l))
  else
    replicate(n_sample, rand_rep_df(seq_n = seq_n, seq_len=seq_l), simplify = FALSE)
}

rand_populations <- function(n_groups, ...) {

  f <- rand_group(...)

  replicate(n_groups, f, simplify = FALSE)
}


