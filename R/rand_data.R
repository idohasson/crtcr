codon_i <- list(c(29,30,31,32,47,48),c(43,44),c(61,62,63,64),c(5,6,7,8,45,46),
     c(49,50,51,52),c(3,4,17,18,19,20),c(33,34,35),c(13,14),c(21,22,23,24),
     c(16),c(41,42),c(37,38,39,40),c(57,58),c(25,26),c(53,54,55,56),c(27,28),
     c(1,2),c(11,12,15),c(9,10),c(59,60),c(36))
AA <- c('R','K','G','S','V','L','I','C','P','W','N','T','D','H','A','Q','F','*','Y','E','M')
coding_i <- which(AA!="*")



# a vector of characters that correspond to the amino acid characters.
codon_table <- c("F", "F", "L", "L", "S", "S", "S", "S",
                 "Y", "Y", "*", "*", "C", "C", "*", "W",
                 "L", "L", "L", "L", "P", "P", "P", "P",
                 "H", "H", "Q", "Q", "R", "R", "R", "R",
                 "I", "I", "I", "M", "T", "T", "T", "T",
                 "N", "N", "K", "K", "S", "S", "R", "R",
                 "V", "V", "V", "V", "A", "A", "A", "A",
                 "D", "D", "E", "E", "G", "G", "G", "G")

coding_tbl_i <- c(1,2,3,4,5,6,7,8,9,10,13,14,16,17,18,19,20,21,22,23,24,25,
              26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,
              45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64)


# codon_i <- list(c(29,30,31,32,47,48),c(43,44),c(61,62,63,64),c(5,6,7,8,45,46),
#                 c(49,50,51,52),c(3,4,17,18,19,20),c(33,34,35),c(13,14),c(21,22,23,24),
#                 c(16),c(41,42),c(37,38,39,40),c(57,58),c(25,26),c(53,54,55,56),c(27,28),
#                 c(1,2),c(9,10),c(59,60),c(36))
# AA <- c('R','K','G','S','V','L','I','C','P','W','N','T','D','H','A','Q','F','Y','E','M')


nt_i <- c(T=0, C=1, A=2, G=3)
to_nt <- c("T","C","A","G")


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

codon_vec_to_nt_string <- function(codon_vec) paste(codon_vec, collapse = "")


# f <- function(i) c((i-1)%/%16, ((i-1)%%16)%/%4, ((i-1)%%16)%%4) + 1


tbl_index_to_codon_indices <- function(tbl_i) {

  # function(i) {i <- i-1; c(bitwXor(i, 15)%/%16, bitwXor(i%%16, 3)%/%4, i%%4)+1}
  tbl_i <- tbl_i - 1

  c(bitwXor(tbl_i, 15)%/%16, bitwXor(tbl_i%%16, 3)%/%4, tbl_i%%4)+1

  #
  # nt1 <- tbl_i %/% 16
  #
  # tbl_i <- tbl_i %% 16
  #
  # nt2 <- tbl_i %/% 4
  #
  # tbl_i <- tbl_i %% 4
  #
  # nt3 <- tbl_i %% 4
  #
  # c(nt1, nt2, nt3) + 1

}

nt_index_to_nt_char <- function(codon_indices) {

  nt_codon <- to_nt[codon_indices]

  paste(nt_codon, collapse = "")

}

tbl_index_to_string_codon <- function(tbl_index) {

  codon_indices <- tbl_index_to_codon_indices(tbl_index)

  nt_index_to_nt_char(codon_indices)

}

rand_clone <- function(n=10, l=13) {



  aa_i <- sample(coding_i, l, replace = TRUE)

  rand_tbl_i <- replicate(n, sapply(codon_i[aa_i], sample, size = 1, replace = T), simplify = F)

  clone_list <- lapply(rand_tbl_i, tbl_index_to_string_codon)

  lapply(clone_list, translate)


  rand_codon_i <- replicate(l, sapply(codon_i[aa_i], sample, size = 1), simplify = FALSE)

  rand_codon_i
  # sapply(rand_codon_i, tbl_indices_vec_to_nt_seq)

}

codon_indices_to_table_index <- function(codon_i) codon_table[codon_i[1] + codon_i[2]*4 + codon_i[3]*16]

# tbl_index_to_codon_indices <- function(tbl_i) c(tbl_i %/% 16, (tbl_i %% 16) %/% 4, tbl_i %% 4)

aa_index <- function(x) codon_table[x[1]*16 + x[2]*4 + x[3] + 1]
# codon_nt_index <- function(nt) vapply(nt, function(x) nt_i[x], integer(1), USE.NAMES = FALSE)


rand_nt <- function(n_codons) {

  coding_codon <- function() {

    codon <- paste(sample(c("A","G","T","C"), 3, rep = TRUE), collapse = "")

    ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)

  }

  paste(replicate(n_codons, coding_codon()), collapse = "")
}

rand_nt_vec <- function(n=10, mu=5) {

  replicate(n, rand_nt(rpois(1,mu)+1))

}

rand_aa <- function(n) {

  AA_list <- c("A","C","D","E","F",
               "G","H","I","K","L",
               "M","N","P","Q","R",
               "S","T","V","W","Y")

  sample(AA_list, n, rep=TRUE) %>%

    paste(collapse = "")
}






rand_rep_vec <- function(seq_type, seq_n = 10, seq_len=13) {

  seq_type <- try(rlang::arg_match(seq_type, c("aa", "nt")))

  rand_f <- ifelse (seq_type == "aa", rand_aa, rand_nt)

  if (length(seq_len)==1) {

    seq_len <- rpois(seq_n, lambda = seq_len)

    seq_len[seq_len==0] <- 1
  }

  sapply(seq_len, rand_f)
}

rand_rep_df <- function(...) {

  clone_vec <- rand_rep_vec(seq_type = "nt", ...)

  cbind.data.frame(nt = clone_vec, aa = translate(clone_vec))

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
