#################### Random Sequences ####################

rand_nt <- function(n_codons) {

  coding_codon <- function() {

    codon <- paste(sample(c("A","G","T","C"), 3, rep = TRUE), collapse = "")

    ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)

  }

  paste(replicate(n_codons, coding_codon()), collapse = "")
}

rand_aa <- function(n) {

  AA_list <- c("A","C","D","E","F",
               "G","H","I","K","L",
               "M","N","P","Q","R",
               "S","T","V","W","Y")

  sample(AA_list, n, rep=TRUE) %>%

    paste(collapse = "")
}

rand_rep_df <- function(...) {

  clone_vec <- rand_rep_vec(seq_type = "nt", ...)

  cbind.data.frame(nt = clone_vec, aa = translate(clone_vec))

}
