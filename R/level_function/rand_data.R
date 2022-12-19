
translate <- function(nt_vec) {
  # checks that each element in the vector is a DNA base.
  stopifnot("All strings must have consecutive triplets of 'A','G','T','C' (uppercase)"=all(grepl(pattern = "^([AGTC]{3})+$", nt_vec)))
  # function for converting a DNA character vector into an amino acid character vector.
  nt_to_aa <- function(nt) {
    # a vector of integers that correspond to the nucleotide characters.
    base_i <- c(T=0, C=1, A=2, G=3)
    # a vector of characters that correspond to the amino acid characters.
    codon_talbe <- c("F", "F", "L", "L", "S", "S", "S", "S",
                     "Y", "Y", "*", "*", "C", "C", "*", "W",
                     "L", "L", "L", "L", "P", "P", "P", "P",
                     "H", "H", "Q", "Q", "R", "R", "R", "R",
                     "I", "I", "I", "M", "T", "T", "T", "T",
                     "N", "N", "K", "K", "S", "S", "R", "R",
                     "V", "V", "V", "V", "A", "A", "A", "A",
                     "D", "D", "E", "E", "G", "G", "G", "G")

    aa_index <- base_i[nt[seq(1,length(nt),3)]] * 16 +
      base_i[nt[seq(2,length(nt),3)]] * 4 +
      base_i[nt[seq(3,length(nt),3)]] + 1

    paste(codon_talbe[aa_index], collapse="")

  }

  vapply(strsplit(nt_vec, NULL), nt_to_aa, character(1))
}

rand_nt <- function(n_codons) {

  coding_codon <- function() {

    codon <- paste(sample(c("A","G","T","C"), 3, rep = TRUE), collapse = "")

    ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)

  }

  paste(replicate(n_codons, coding_codon()), collapse = "")
}
# rand_nt_vec(l=3)
rand_nt_vec <- function(n=100, l=1) {

  replicate(n, rand_nt(sample(l,1)))

}


rand_nt_list <- function(n=100, l=1, v=3) {
  replicate(v, rand_nt_vec(n, l), simplify = FALSE)
}


rand_aa <- function(n=100) {

  AA_list <- c("A","C","D","E","F",
               "G","H","I","K","L",
               "M","N","P","Q","R",
               "S","T","V","W","Y")

  sample(AA_list, n, rep=TRUE) %>%

    paste(collapse = "")
}
# rand_rep_df()
rand_rep_df <- function(n=100, l=1, rid=8, gid=2) {

  clone_vec <- rand_nt_vec(n, l)

  clonotype_vec <- translate(clone_vec)

  id1_vec <- sample(paste0("id", seq(rid)), n, replace = TRUE)

  id2_vec <- ifelse(id1_vec %in% paste0("id", seq(rid))[stats::rnorm(8)>0], "cancer", "clontrol")

  # id2_vec <- rlang::rep_along(clone_vec, )

  # id1_vec <- rlang::rep_along(clone_vec, paste0("id", seq(rid)))

  # id2_vec <- rlang::rep_along(clone_vec, paste0("ID", seq(gid)))

  data.frame(nt=clone_vec, aa=clonotype_vec, id=id1_vec, id2=id2_vec)

}

rand_group <- function(n_sample=5, seq_n = rpois(n_sample, 1E3), seq_l=3) {
  if (length(seq_n) > 1)
    lapply(seq_n, function(n) rand_rep_df(seq_n = n, seq_len=seq_l))
  else
    replicate(n_sample, rand_rep_df(seq_n = seq_n, seq_len=seq_l), simplify = FALSE)
}

rand_populations <- function(n_groups=2, ...) {

  f <- rand_group(...)

  replicate(n_groups, f, simplify = FALSE)
}
# rand_clone_list()
rand_clone_list <- function(clone_number=100, codon_size=1) {

  nt_vec <- replicate(clone_number, rand_nt(codon_size))

  aa_vec <- translate(nt_vec)

  split(nt_vec, aa_vec)

}
