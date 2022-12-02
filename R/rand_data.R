
nt_i <- c(T=0L, C=1L, A=2L, G=3L)

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


codon_i <- list(R=c(29,30,31,32,47,48),K=c(43,44),G=c(61,62,63,64),c(5,6,7,8,45,46),
                c(49,50,51,52),c(3,4,17,18,19,20),c(33,34,35),c(13,14),c(21,22,23,24),
                c(16),c(41,42),c(37,38,39,40),c(57,58),c(25,26),c(53,54,55,56),c(27,28),
                c(1,2),c(9,10),c(59,60),c(36))
AA <- c('R','K','G','S','V','L','I','C','P','W','N','T','D','H','A','Q','F','Y','E','M')





codon_indices_to_table_index <- function(codon_i) codon_i[1]*16 + codon_i[2]*4 + codon_i[3]

tbl_index_to_codon_indices <- function(tbl_i) c(tbl_i %/% 16, (tbl_i %% 16) %/% 4, tbl_i %% 4)

# aa_index <- function(x) codon_table[x[1]*16 + x[2]*4 + x[3] + 1]
# codon_nt_index <- function(nt) vapply(nt, function(x) nt_i[x], integer(1), USE.NAMES = FALSE)
# rand_codon <- function(n)
# codon_nt_index(c("T", "A"))




aa_index <- nt_i[nt[seq(1,length(nt),3)]] * 16 +
            nt_i[nt[seq(2,length(nt),3)]] * 4 +
            nt_i[nt[seq(3,length(nt),3)]] + 1





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


rand_clone <- function(n=10, l=13) {

  codon_i <- list(c(29,30,31,32,47,48),c(43,44),c(61,62,63,64),c(5,6,7,8,45,46),
       c(49,50,51,52),c(3,4,17,18,19,20),c(33,34,35),c(13,14),c(21,22,23,24),
       c(16),c(41,42),c(37,38,39,40),c(57,58),c(25,26),c(53,54,55,56),c(27,28),
       c(1,2),c(11,12,15),c(9,10),c(59,60),c(36))
  aa <- c('R','K','G','S','V','L','I','C','P','W','N','T','D','H','A','Q','F','*','Y','E','M')
  base_i <- c(T=0, C=1, A=2, G=3)

  AA_list <- c("A","C","D","E","F",
               "G","H","I","K","L",
               "M","N","P","Q","R",
               "S","T","V","W","Y")

  codon_talbe <- c("F", "F", "L", "L", "S", "S", "S", "S",
                   "Y", "Y", "*", "*", "C", "C", "*", "W",
                   "L", "L", "L", "L", "P", "P", "P", "P",
                   "H", "H", "Q", "Q", "R", "R", "R", "R",
                   "I", "I", "I", "M", "T", "T", "T", "T",
                   "N", "N", "K", "K", "S", "S", "R", "R",
                   "V", "V", "V", "V", "A", "A", "A", "A",
                   "D", "D", "E", "E", "G", "G", "G", "G")


  sample(AA_list, l, replace = TRUE)

  codon_i <- which(codon_talbe %in% "F")



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
