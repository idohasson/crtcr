# length.seqs <- 100 # length of DNA sequence
# nucl.freqs <- rep(1/4, 4) # nucleotide frequencies
# nucl <- c('a', 'c', 'g', 't') # A, C, G, T
#
# seqs <- sample(nucl, size = length.seqs, replace = TRUE, prob = nucl.freqs)
#
# bad_codons <- c("aga", "agg", "taa", "tag", "tga")
#
# regx <- paste0("(", paste(bad_codons, collapse = ")|("), ")")
#
# s <- paste(seqs, collapse = "")
#
# while( grepl(regx, s) ) {
#   s <- gsub(regx,
#             paste(sample(nucl, size = 3, replace = TRUE, prob = nucl.freqs), collapse = ""),
#             s)
# }
#
# s
#



library(agvgd)
# package ‘segmented’
# package ‘grantham’
# package ‘seqinr’
# package ‘agvgd’
library('readr')

DIR_BETA = "data/Beta/"

dir_path <- DIR_BETA
paths <- list.files(dir_path, full.names=TRUE)
startsWith("BRCA", basename(paths))

group_file_name_prefix <- "BRCA"
group1_paths <- paths[startsWith(basename(paths), group_file_name_prefix)]


mixcr_column_type <- cols(
  .default = col_logical(),
  cloneId = col_double(),
  cloneCount = col_double(),
  cloneFraction = col_double(),
  targetSequences = col_character(),
  targetQualities = col_character(),
  allVHitsWithScore = col_character(),
  allDHitsWithScore = col_character(),
  allJHitsWithScore = col_character(),
  allCHitsWithScore = col_character(),
  allVAlignments = col_character(),
  allDAlignments = col_character(),
  allJAlignments = col_character(),
  allCAlignments = col_character(),
  nSeqCDR3 = col_character(),
  minQualCDR3 = col_double(),
  aaSeqCDR3 = col_character(),
  refPoints = col_character()
)
rep1 <- read_tsv(group1_paths[1], col_types = mixcr_column_type)

grouped_aa <- split(rep1$nSeqCDR3, rep1$aaSeqCDR3)
unique(rep1$aaSeqCDR3) %in% names(grouped_aa) %>% all

grouped_nt <- split(rep1$aaSeqCDR3, rep1$nSeqCDR3)
unique(rep1$nSeqCDR3) %in% names(grouped_nt) %>% all



library(tools)
group_file_name_prefix <- "Control"
group2_paths <- paths[startsWith(basename(paths), group_file_name_prefix)]
names(group1_paths) <- file_path_sans_ext(basename(group1_paths))
names(group2_paths) <- file_path_sans_ext(basename(group2_paths))

group1 <- group1_paths %>% lapply(read_tsv, col_types = mixcr_column_type)
group2 <- group2_paths %>% lapply(read_tsv, col_types = mixcr_column_type)


# rep1 %>% select(cdr3_nt_col, cdr3_aa_col) %>%
#   group
library(dplyr)
cdr3_nt_col <- "nSeqCDR3"
cdr3_aa_col <- "aaSeqCDR3"
col_names <- c(cdr3_nt_col, cdr3_aa_col)



rep <- group1[[1]][col_names]

nt_col <- rep[cdr3_nt_col]
aa_col <- rep[cdr3_aa_col]

nt_vec <- unlist(nt_col, use.names = FALSE)
aa_vec <- unlist(aa_col, use.names = FALSE)

split(nt_vec, aa_vec)  # split 2 vectors

user_input <- rep



# for 2 col input
if (is.data.frame(user_input) & ncol(nt_col)==1) {
  print("asd")
}


split_2_vec <- function(vec1, vec2) {
  split(nt_vec, aa_vec)
}


split_by_col <- function(df, col1, col2) {
  vec1 <- unlist(col1, use.names = FALSE)
  vec2 <- unlist(col2, use.names = FALSE)
  split_2_vec(vec1, vec2)
}

# split_by_col <- function(df, )

# mutual AA sequences
public_seq <- intersect(names(grouped_aa), names(x))
# list to df
unsplit(x, public_seq) %>% length









pair_list <- group1 %>% names %>% combn(2, simplify = FALSE)




