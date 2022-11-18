##################### Repertoire Data #####################
rep_df <- read.delim(paths[1], sep = "\t", header = TRUE)
# head(rep_df)
aa_var <- "aaSeqCDR3"; nt_var <- "nSeqCDR3";
v_var <- "allVHitsWithScore"; j_var <- "allJHitsWithScore";

extract_seg <- function(v_str) strsplit(v_str, split = "\\*")[[1]][1]

rep_df2 <- rep_df[,c(aa_var, nt_var)] %>%
  cbind(allVHitsWithScore=map_chr(rep_df[,v_var], extract_seg),
        allJHitsWithScore=map_chr(rep_df[,j_var], extract_seg))
head(rep_df2)

tbl <- table(rep_df2$aaSeqCDR3)

rep_df3 <- rep_df2[rep_df2$aaSeqCDR3 %in% names(tbl[(tbl)>1]),]

clone_vars <- c(nt_var, j_var); clonotype_vars <- c(aa_var, v_var);

by(rep_df3[clone_vars], rep_df3[aa_var], unique, simplify = FALSE) %>% as.list.data.frame()

nt2codon <- function(nt_vec) {

  # if (missing(func))
  #   func <- as_function(~)
    # func <- as_function(~paste(., collapse = ""))
  INDEX=c(T=0L,C=1L,A=2L,G=3L)

  codon2aa_i <- function(codon)
    INDEX[codon[1]]*16L + INDEX[codon[2]]*4L + INDEX[codon[3]] + 1L

  v2i <- function(codon_vec)
    vapply(codon_vec, codon2aa_i, integer(1), USE.NAMES = FALSE)

  get_coodon <- function(s)
    tapply(s, gl(length(s)%/%3,3), v2i)

  map(strsplit(nt_vec, ""), get_coodon)

}



# rep_df3$nSeqCDR3[1:3] %>% nt2codon(func = paste, collapse = "")

# * c(1,4,16)
# c(T = 0, C = 1, A = 2, G = 3)






# vapply(codon2aa_i, double(1), USE.NAMES = FALSE)

rep_df2$nSeqCDR3[1:3] %>%
  nt2codon()



decoding <- function(codon) INDEX[codon]
f <- function(codons) {
  vapply(codons, decoding, integer(1), USE.NAMES = FALSE)
}





# list(c("A", "G", "T"))

rep_df3$nSeqCDR3[1:3] %>%
  # map(~nchar())
  nt2codon()

  # strsplit("") %>%
  # map(get_coodon)
  # # map(. %>% tapply(gl(length(.)%/%3,3), paste, collapse = ""))
  #














##################### Group Data #####################



##################### Population Data #####################





get_clone_attr <- function(l, clone_attr) map(l, clone_attr)

flat_clonotypes_chr <- function(l) flatten_chr(map(l, unique))

flat_clonotypes_df <- function(df) flatten_df(map(df, unique))

cr_level_nt <- function(nt) tapply(nt, translate(nt), n_unique_f)

cr_level_nt <- function(nt) tapply(nt, translate(nt), mean_unique_f)



# function or lambda formula to apply on
# each sub set of NT sequences encoding
# a specific AA sequence.
#
# tapply(nt_vec, translate(nt_vec), func)
#
# Example:
#
#
#
n_unique_f <- ~ length(unique(.x))
func <- rlang::as_function(n_unique_f)

map_func <- as_mapper(~ tapply(.x, translate(.x), func))

map(get_clone_attr(dfl[1:3], nt), map_func)



# v <- get_clone_attr(dfl[1], nt)[[1]]
# # total number of NT sequences encoding a specific AA sequence
# f1 <- nt_vec_f(length)
# f1(v)
# # average number of unique NT sequences encoding a specific AA sequence
# f2 <- nt_vec_f(~mean(table(.x)))
# f2(v)
nt_vec_f <- function(lambda_func) {
  func <- rlang::as_function(lambda_func)
  rlang::as_function(~ tapply(.x, translate(.x), func))
}

# l <- get_clone_attr(dfl[1:3], nt)
# # total number of NT sequences encoding a specific AA sequence
# f3 <- nt_vec_map_f(~length(unique(.x)))
# map(l, f3)
# # average number of unique NT sequences encoding a specific AA sequence
# my_cool_func <- function(x) max(table(x))
# f4 <- nt_vec_map_f(my_cool_func)
# map(l, f4)
nt_vec_map_f <- function(lambda_func) {
  v_func <- nt_vec_f(lambda_func)
  as_mapper(v_func)
}




nt_vec_f(length)(get_clone_attr(dfl[[1]], nt))
map(get_clone_attr(dfl[1:3], nt), nt_list_f(length))

n_unique_f <- ~ length(unique(.x))
# n_unique_f <- as_mapper(~ length(unique(.x)))
mean_unique_f <- as_mapper(~ mean(table(.x)))

get_clone_attr(dfl, nt)

  flat_clonotypes_chr()


f_new <- nt_f(n_unique_f)

get_clone_attr(dfl[1:3], nt)

  # map(as_mapper(~ tapply(.x, translate(.x), rlang::as_function(n_unique_f))))

# map(as_mapper(~ f_new(.x)))

# tapply(., translate(.), length)





# as_mapper(~ length(unique(.x)))






# dfl[1:3] %>% map(as.list) %>% map_chr(as_mapper(~ aa))




is_public <- function(clonotype_attr) {
  map_chr(clonotype_attr, ~anyDuplicated.data.frame(.x))
}

anyDuplicated.data.frame()

dir_path <- getwd() %>% paste("/in-development/beta_MiXCR/", sep = "")

paths <- list.files(dir_path, full.names = TRUE)

dfl <- map(paths, read.delim, nrows = 10)

nt_list <- get_clone_attr(dfl[1:3], "nSeqCDR3")

nt <- "nSeqCDR3"; aa <- "aaSeqCDR3"; id <- "r_id"

func <- as_mapper(~ translate(unique(.x)))

nt_list %>%

  map(func) %>%

  flatten_chr() %>%

  .[duplicated(.)]


# func <- as_mapper(~ length(unique(.x)))

# map(nt_list, func)
# map_chr(nt_list[[1]], translate)

# map_chr()
# translate()


# tapply(attr1[nt], attr1[c(id, aa)], unique)
