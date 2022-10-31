
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
#' @export
#'
#' @examples
rand_group <- function(rep_type, n_sample, ..., named=TRUE) {

  rep_ <- type <- try(rlang::arg_match(rep_type, c("aa_vector", "nt_vector", "df_list")))

  if (rep_type == "aa_vector") {

    rand_f <- function() rand_rep_vec(seq_type="aa", ...)

  } else if (rep_type == "nt_vector") {

    rand_f <- function() rand_rep_vec(seq_type="nt", ...)

  } else if (rep_type == "df_list") {

    rand_f <- function() rand_rep_df(...)
  }

  replicate(n_sample, rand_f(), simplify = FALSE) %>%

    setNames(paste0("rep", seq_along(.)))

}
