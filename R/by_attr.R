library(rlang)
library(vctrs)
library(tibble)
library(tidyr)
library(magrittr)

cr(clonal_sequence, source)


crSeq <- function(clonotype_seq, clonal_seq) {

  is_character(clonotype_seq, n = 1)

  if (is_crSeq(clonotype_seq))
    # add CR sequence if already exits
    clonal_seq <- append(clonal_seq, get_crSeq(clonotype_seq), after = 0)

  attr(clonotype_seq, "crSeq") <- unique(clonal_seq)

  clonotype_seq

}



crSeq.repertoire <- function(clonotype_seq, clonal_seq) {

  clonotype_list <- vec_split(clonal_seq, by = clonotype_seq)

  mapply(crSeq, clonotype_list$key, clonotype_list$val,

         SIMPLIFY = FALSE, USE.NAMES = FALSE)


}



is_crSeq <- function(clonotype_seq) {

  !is_null(get_crSeq(clonotype_seq))

}

get_crSeq <- function(clonotype_seq) {

  attr(clonotype_seq, "crSeq")

}



shareSeq <- function(clonotype_seq, repertoire_id) {

  is_character(clonotype_seq, n = 1)

  if (is_shareSeq(clonotype_seq))
    # add IDs if already exits
    repertoire_id <- append(repertoire_id, get_shareSeq(clonotype_seq), after = 0)

  attr(clonotype_seq, "rID") <- unique(repertoire_id)

  clonotype_seq


}

is_shareSeq <- function(clonotype_seq) {
  !is_null(get_shareSeq(clonotype_seq))
}

get_shareSeq <- function(clonotype_seq) {

  attr(clonotype_seq, "rID")

}



cr_level <- function(clonotype_seq) {

  sequences <- get_crSeq(clonotype_seq)

  if (is_null(sequences)) print("error")

  length(sequences)

}
share_level <- function(share_seq) {

  if (!is_shareSeq(share_seq))
    print("ERROR - no rID attribute")

  sequences <- attr(share_seq, "rID")

  length(sequences)

}

# Define the nucleotide sequences
nucleotides <- c("ATGCTAGCAT", "ATGCTAGCAT", "ATGCTAGCAT", "ATGCTAGCAA", "ATGCTAGCAG")

# Define the reference protein sequence
reference <- "MCTAC"

cr_seq1 <- crSeq(reference, nucleotides)

is_crSeq(cr_seq1)

get_crSeq(cr_seq1)

cr_level(cr_seq1)


# Define the repertoire identifier the protein sequence belong to.
repertoire_identifiers <- paste("SAMPLE", LETTERS[c(1,1,2,2,2)], sep="_")

share_seq1 <- shareSeq(reference, repertoire_identifiers)

is_shareSeq(share_seq1)

get_shareSeq(share_seq1)

share_level(share_seq1)


share_cr_seq1 <- shareSeq(cr_seq1, repertoire_identifiers)

is_crSeq(share_cr_seq1)

is_shareSeq(share_cr_seq1)


get_crSeq(share_cr_seq1)

get_shareSeq(share_cr_seq1)


cr_level(share_cr_seq1)

share_level(share_cr_seq1)



(r1 <- replicate(1000, rand_nt(2)))
(c1 <- translate(r1))
(cr1 <- crSeq.repertoire(c1, r1))
(cr1 <- crSeq.repertoire(c1, r1))

tmp <- clonotype[1:10,]
clonotype <- vec_split(r1, c1)
mapply(crSeq, tmp$key, tmp$val, SIMPLIFY = F, USE.NAMES = FALSE) %>% sapply(is_crSeq)
tmp
lapply(clonotype$key, crSeq)
r1



# clones <- rand_clones(1000,2)
# which.max(sapply(clones, function(x) length(unique(x))))

clonotype <- "RR"
clone <- clones[[clonotype]]
ID <- rep_along(clone, paste0("R", seq(4)))


clone_g1 <- clone[1:(length(clone)%/%2+1)]
clone_g2 <- clone[(length(clone_g1)+1):length(clone)]


id_g1 <- ID[ID=="R1"|ID=="R2"]
id_g2 <- ID[!(ID=="R1"|ID=="R2")]

clonotype_g1 <- shareSeq(crSeq(clonotype, clone_g1), id_g1)
clonotype_g2 <- shareSeq(crSeq(clonotype, clone_g2), id_g2)



clonotype_list <- list(clonotype_g1, clonotype_g2)

lapply(clonotype_list, cr_level)
lapply(clonotype_list, share_level)

#
repertoires <- replicate(12, rand_clones(100,2), F)
unlist(repertoires, recursive = FALSE)[["RR"]]
enframe(lapply(repertoires, enframe), name = ".rid", value = ".clonotypes")
group_tbl <- enframe(repertoires, name = ".rid", value = ".clonotypes")
pivot_longer(group_tbl, cols = .clonotypes, names_to = ".cid")

group_tbl$clonotypes

#

tbl <- enframe(clones, name = ".clonotype", ".clone")[1:3,]
tbl[".rid"] <-
clones












