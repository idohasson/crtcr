
# check data fits definition

# data has unique pairing of clonotype and clonal sequence

# data frame

check_clonotype_df <- function(df, clone, clonotype) {

  stopifnot(is.data.frame(df))

  distinct_at(df, c(clone, clonotype))

}

# check_clonotype

# check_clonotype_n

# check_clonotype_v

# check_clonotype_
