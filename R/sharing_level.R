library("purrr")

# number of vectors each sequence presented in
sharing_levle <- function(seq_list1, seq_list2, ...) {

  unique_clonotypes <- map(c(seq_list1, seq_list2, ...), unique)

  clonotype_collection <- unlist(unique_clonotypes, use.names = FALSE)

  table(clonotype_collection)
}

sharing_level_per_gruop <- function(dtl) {
  acast(dtl,
        aaSeqCDR3 ~ group,
        value.var = "sample",
        fun.aggregate = n_distinct)
}


sharing_df <- function(seq_list, by_col) {
  # Check if vector type
  stopifnot("'seq_list' must be a vector"=is.vector(seq_list))

  # Check if vector type
  stopifnot("'by_col' must be a vector of dataframes" = is.character(by_col) & length(by_col) == 1)

  # Check that all values are data-frames and each has the column name
  colname_in_df <- lapply(seq_list, function(df) by_col %in% names(df))
  stopifnot("'by_col' counldn't be found in one of the dataframes"=all(as.logical(colname_in_df)))

  # Make the combinations of list elements
  pairs <- combn( seq_list , 2 , simplify = FALSE )
  # Intersect the list elements
  out <- lapply( pairs , function(x) intersect( x[[1]][by_col] , x[[2]][by_col] ) )

  nms <- combn( names(seq_list) , 2 , FUN = paste0 , collapse = "," , simplify = FALSE )
  return(setNames( out , nms ))
}
