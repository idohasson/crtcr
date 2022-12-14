library(rlang)
library(vctrs)
library(tibble)

# convergent_recombination_level <- function()
# NT=rand_nt_vec(100, 1);AA=translate(NT);ID=rep_along(NT, 1:4);FREQ=runif(100);DF=data.frame(nt=NT, aa=AA, id=ID, freq=FREQ)

# is_cr(c("ATC", "ATT"))
is_cr <- function(nt) {



  aa <- lapply(nt, translate)

  Reduce(`==`, aa)

}

split_cr <- function(nt) {

  aa <- translate(nt)

  split(nt, aa)

}

unique_cr <- function(nt) {

  aa <- translate(nt)

  cr_list <- split(nt, aa)

  lapply(cr_list, unique)

}

count_cr <- function(nt) {

  aa <- translate(nt)

  unique_aa <- unique(aa)

  length(unique_aa)

}


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




# cr_number(NT[2:5],NT[5:7])
cr_number <- function(...) {

  nt_list <- dots_splice(...)

  vapply(nt_list, vec_unique_count, integer(1))
  # do.call(vec_unique_count, nt_list)

}

cr_level <- function(..., group_level=mean) {

  levels <- cr_number(...)

  levels

}

group_cr_level <- function(..., group_by) {

  dfl <- df_list(..., .name_repair = "unique_quiet")

  args <- new_data_frame(dfl)

  args

  # indices <- vec_group_loc(args)
  # grouped_nt <- vec_chop(.nt, indices$loc)
  # levels <- cr_level(grouped_nt)
  # levels

}

tbl <- with(df, tapply(nt, list(aa, id, id2), cr_number, default = 0))



table(df[df$aa=="S",c("nt", "id")])
prop.table(table(df[df$aa=="S",c("nt", "id2")]))
prop.table(table(df[df$aa=="S",c("id", "id2")]))
split(df$nt, df$aa)[["A"]]


prop.table(table(vec_count(df[c("nt", "id")])$key), margin = 1)

f1 <- function(x) cr_number(x)
f2 <- function(x) prop.table(table(x), margin = 1)

tbl1 <- prop.table(tapply(df$nt, df[c("aa", "id")], cr_number, default = 0), margin = 1)
tbl2 <- prop.table(tapply(df$id2, df[c("aa", "id")], cr_number, default = 0), margin = 1)

heatmap(as.matrix.data.frame(apply(tbl1, 1, quantile) / apply(tbl2, 1, quantile)))


sapply(split(df$nt, df$aa), f2)
sapply(split(df$id, df$aa), f)


prop.table(table(df$id))


c1 <- vec_count(df$aa, sort = "location")
q <- mapply(rep, level(c1$count, ~unique(quantile_levels(.x))), c1$count)
i <- vec_group_loc(df$aa)
(g <- list_unchop(q, indices = i$loc))


(cring <- cr_number(with(df, split(nt, aa))))
(sharing <- cr_number(with(df, split(id, aa))))

plot(quantile(proportions(cring), probs = c(0, 0.25, 0.5, 0.75, 1)) - quantile(proportions(sharing), probs = c(0, 0.25, 0.5, 0.75, 1)))

cr_level <- function(nt, aa=NULL, named=FALSE) {

  cr_seq <- cr_list(nt, aa, named)

  vapply(cr_seq, vec_unique_count, integer(1))

}


group_cr_level <- function(nt, ..., group_level=mean) {

  dfl <- df_list(..., .name_repair = "minimal")

  df <- new_data_frame(dfl)

  cr_df <- vec_split(nt, df)

  levels <- vapply(cr_df$val, cr_level, integer(1))

  group_level(levels)

}

cr_level <- function(nt, ...) {

  nt_list <- dots_splice(nt, ...)

  vapply(nt_list, vec_unique_count, integer(1))

}



# nt_list <- replicate(4, sample(S, sample(5,1), rep=T))
# group_cr_level(nt_list)
# group_cr_level(nt_list, group_func = mean)
# group_cr_level(nt_list, group_func = NULL)
# DF_R <- DF[DF$aa=="R", c("nt", "aa", "id")]
# grouped_nt <- split(DF_R$nt, DF_R$id)
# group_cr_level(grouped_nt, ~mean(level(.x, median)))
group_cr_level <- function(nt_list, group_level=mean) {

  if (is_formula(group_level))

    group_level <- as_function(group_level)

  if (is_function(group_level)) {

    levels <- cr_level(nt_list)

    group_level <- group_level(levels)

  } else if (is_null(group_level)) {

    group_nt <- unlist(nt_list, use.names = FALSE)

    group_level <- cr_level(group_nt)

  } else stop("group_func must be either a function, formula or NULL")

  return(group_level)
}

cr_level_df <- function(nt, aa=NULL) {

  # vec_cbind(cr_df$key, CRlevel=level)
  cr_v <- clonorype_cr_level(nt, aa, named = TRUE)

  tbl <- tibble::enframe(cr_v, "clonotype", "CRlevel")

  as.data.frame(tbl)



}

# add_cr_level(DF, "nt", "aa")
add_cr_level <- function(df, nt_field=1, aa_field=NULL) {

  nt <- field(df, nt_field)

  aa <- if (is_null(aa_field)) {translate(nt)} else {field(df, aa_field)}

  cr_seq <- vec_group_loc(aa)

  cr_list <- vec_chop(nt, cr_seq$loc)

  each_row_level <- function(x) rep_along(x, vec_unique_count(x))

  cr_levels <- lapply(cr_list, each_row_level)

  levels_vec <- list_unchop(cr_levels, indices = cr_seq$loc)

  vec_cbind(df, CRlevel=levels_vec)

  # vec_cbind(clonotype=aa, clone=nt, CRlevel=levels_vec)

}

# cr_list(rand_nt_vec(100,1))
cr_list <- function(nt, aa=NULL, named=FALSE) {

  is_cr <- !duplicated(nt)

  nt <- nt[is_cr]

  if (is_null(aa)) {

    aa <- translate(nt)

  } else {

    aa <- aa[is_cr]

  }

  cr_split <- vec_split(nt, aa)

  if (isTRUE(named)) {

    return(tibble::deframe(cr_split))

  } else {

    return(cr_split$val)

  }

}
