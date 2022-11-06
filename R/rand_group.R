

#' random list of NT vector sequnces
#'
#' @param rep_n number of vector
#'
#' @return list of vector
#'
#' @importFrom stats rpois
#'
#' @export
#'
#' @examples
#'
#' # also random group size for no rep_n arg
#' rand_group()
#'
rand_group <- function(rep_n=rpois(1, 10)) {

  seq_n <- sample(1E2:1E3, rep_n)

  clone_gen <- function() {

    coding_seq <- function(n_codons) {

      coding_codon <- function() {

        codon <- paste(sample(c("A","G","T","C"), 3, replace = TRUE), collapse = "")

        ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)

      }

      paste(replicate(n_codons, coding_codon()), collapse = "")
    }

    Vectorize(coding_seq, "n_codons")
  }

  nt_gen <- clone_gen(); l_mu <- 3

  lapply(seq_n, function(n) nt_gen(rpois(n, l_mu)+1))

}

