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
# library(agvgd)
# package ‘segmented’
# package ‘grantham’
# package ‘seqinr’
# package ‘agvgd’

split_by_col <- function(df, cdr3_nt_col, cdr3_aa_col) {
  rep <- df[c(cdr3_nt_col, cdr3_aa_col)]
  colnames(rep) <- c("nt", "aa")
  aa_to_nt <- split(rep$nt, rep$aa)
  # split 2 vectors in a data frame
  # resulting a named list mapping
  # NT sequences groups to their
  # mutual encoding CDR3 AA sequence
  return(aa_to_nt)
}


code <- c(T = 0, C = 1, A = 2, G = 3)
aa_code <- "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"
aa_code <- strsplit(aa_code, "")[[1]]
translate <- function(nt_seq) {
  c <- code[nt_seq]
  in_frame <- seq(from=1, to=length(c) ,by=3)
  c <- 16*c[in_frame] + 4*c[in_frame + 1] + c[in_frame + 2] + 1
  paste0(aa_code[c], collapse = "")

}
nt_to_aa <- function(nt_vec) {
    nt_vec %>%
    as.vector %>%
    strsplit(split = "") %>%
    map(translate) %>%
    unlist
}


nt_col_name <- "nSeqCDR3"
aa_col_name <- "aaSeqCDR3"

df_list %>%
  # lapply(select, nt_col_name, aa_col_name) %>%
  lapply(pull, var = nt_col_name, name = aa_col_name) %>%
  Reduce(f = union)


df_list %>%
  lapply(distinct, c(nt_col_name, aa_col_name), .id = "sample")

lapply(df_list, pull, var = "nSeqCDR3", name = "aaSeqCDR3") %>%
  map(unsplit(is.element))

  # unsplit(is.element)


aa_list <- lapply(df_list, pull, "aaSeqCDR3")
nt_list <- lapply(df_list, pull, "nSeqCDR3")
mapply(split, nt_list, aa_list,SIMPLIFY = FALSE)


df %>% split(~ nSeqCDR3)



source("R/scripts/read_rep.R")

nt_col_name <- "nSeqCDR3"
aa_col_name <- "aaSeqCDR3"
from_cols <- c(aa_col_name, nt_col_name)

aa_count <- table(data, dnn = "sample")
aa <- split(data, ~ aaSeqCDR3)

rep_list <- split(data, ~ sample)
all_aa <- Reduce(union, rep_list)

share_seq <- function(df1, df2, bycol) semi_join(df1, df2, by = bycol)
aa_intersect <- function(pair) mapply(intersect, pair[1], pair[2])

grouped_aa <- split(rep1$nSeqCDR3, rep1$aaSeqCDR3)
unique(rep1$aaSeqCDR3) %in% names(grouped_aa) %>% all

grouped_nt <- split(rep1$aaSeqCDR3, rep1$nSeqCDR3)
unique(rep1$nSeqCDR3) %in% names(grouped_nt) %>% all




# fruits <- tibble(
#   type   = c("apple", "orange", "apple", "orange", "orange", "orange"),
#   year   = c(2010, 2010, 2012, 2010, 2011, 2012),
#   size  =  factor(
#     c("XS", "S",  "M", "S", "S", "M"),
#     levels = c("XS", "S", "M", "L")
#   ),
#   weights = rnorm(6, as.numeric(size) + 2)
# )
#
# # All possible combinations ---------------------------------------
# # Note that all defined, but not necessarily present, levels of the
# # factor variable `size` are retained.
# fruits %>% expand(type)


# aa_to_nt <- split(rep$nt, rep$aa) # split 2 vectors in a data frame
                                  # resulting a named list mapping
                                  # NT sequences groups to their
                                  # mutual encoding CDR3 AA sequence




?crossing(var1 = 0:1, var2 = 0:1, var3 = 0:1)

n <- 14
l <- rep(list(0:1), n)
?expand.grid(l)






# intersect(rep_list$BRCA1, rep_list$BRCA2)
# intersect(unname(rep_list)[[1]], unname(rep_list)[[2]])

# all_aa <- unlist(lapply(rep_list, names), use.names = TRUE)

# which(table(all_aa) > 1, arr.ind = TRUE)

# length(unique(rep_list$BRCA1))


all_cdr3 <- Reduce(c, rep_list) # Function clusterMap and mcmapply (not Windows) in package parallel provide parallel versions of Map.





combn(unname(lapply(rep_list, names)), 2, simplify = FALSE, intersect)
lapply(rep_list, names)
lapply(rep_list, )



rep1 <- split_by_col(group1$BRCA1, "nSeqCDR3", "aaSeqCDR3")
rep2 <- split_by_col(group2$Control1, "nSeqCDR3", "aaSeqCDR3")


mapply(c, rep1, rep2, SIMPLIFY=FALSE)

?Reduce(union)

intersecting_aa <- intersect(names(rep1), names(rep2))

nt_vec <- unlist(rep$nt, use.names = FALSE)
aa_vec <- unlist(aa_col, use.names = FALSE)

unlist(group1[[1]][cdr3_nt_col], use.names = FALSE)
unlist(group1[[1]][cdr3_nt_col], use.names = FALSE)


nt_col <- rep[cdr3_nt_col]
aa_col <- rep[cdr3_aa_col]


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




