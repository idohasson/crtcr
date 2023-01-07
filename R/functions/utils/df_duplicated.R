# df <- rand_rep_df()
# (df_d <- df_duplicated(df, aa, id))
# class(x) <- NULL;x
df_duplicated <- function(data, aa, ...) {

  if (!missing(...)) data <- data_group(data, ...)

  data_tabulate(data, aa,collapse = TRUE)
}

df_unique

data_unique(df)
