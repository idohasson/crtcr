#' Amount of unique values overall.
#'
#' @param ... Same length vectors of values.
#' @param na.rm if TRUE, ignore aligned values if any of them are NA. For FALSE,
#' the default, NA is treated as an unique value.
#'
#' @return An integer of unique value count.
#' @export
#'
#' @examples
#'
#' v1 <- c("A", "A", "A", "B")
#' v2 <- factor(c("seqA", "seqA", "seqB", "seqB"))
#' v3 <- c(1,NA,2,2)
#'
#' unique_n(v1)
#' unique_n(v1, v2)
#' unique_n(v1, v2, v3) # NA in v3 is considered as a unique value.
#' unique_n(v1, v2, v3, na.rm = TRUE)
#'
unique_n <- function(..., na.rm = FALSE) {

  dfl <- df_list(..., .name_repair = "minimal")

  dfi <- new_data_frame(dfl)

  if (isTRUE(na.rm)) {

    no_na <- vec_detect_complete(dfi)

    dfi <- vec_slice(dfi, no_na)

  }

  vec_unique_count(dfi)

}








n_unique_df <- function(df, na.rm=FALSE) {

  if (isTRUE(na.rm)) {

    no_na <- vec_detect_complete(df)

    dfi <- vec_slice(dfi, no_na)

  }

  vec_unique_count(df)

}

n_unique_vec <- function(...) {

  if (rlang::dots_n(...) == 1) {

    v <- c(...)

    if (is.numeric(v))

      return(n_unique_num(v))

    else

      return(vec_unique_count(v))

  }

  dfl <- df_list(..., .name_repair = "minimal")

  dfi <- new_data_frame(dfl)

  n_unique_df(dfi)

}

n_unique_list <- function(..., each_flat=FALSE, flat_num=FALSE) {

  l <- rlang::dots_splice(...)

  if (isTRUE(each_flat)) {

    l <- lapply(l, unlist, use.names = FALSE)

    n <- vapply(l, n_unique_vec, integer(1L))

    if (isTRUE(flat_num))

      return(n_unique_num(n))

    else

      return(n)

  } else {

    l <- unlist(l, use.names = FALSE)

    return(n_unique_vec(l))

  }





}

n_unique_lgc <- function(logical_vec, na.rm = FALSE) {

  if (isTRUE(na.rm))

    logical_vec <- logical_vec & vec_detect_complete(logical_vec)

  sum(logical_vec)

}

n_unique_num <- function(num_vec, na.rm = FALSE) {

  lgc <- as.logical(num_vec)

  n_unique_lgc(lgc, na.rm)

}

n_unique_arr <- function(num_array, margins=1, na.rm = FALSE) {

  apply(num_array, margins, n_unique_num, na.rm)

}

n_unique <- function(x, ...) {

  if (is.numeric(x))

    if (is_dim(x))

      return(n_unique_arr(x, ...))

  else

    return(n_unique_num(x, ...))

  if(is.vector(x))

    if (is.list(x))

      return(n_unique_list(x, ...))

  else

    return(n_unique_vec(x, ...))

  else if (is.data.frame(x, ...))

    return(n_unique_df(x, ...))

  else

    stop("Unsupported input type")

}

is_dim <- function(x) {

  d <- dim(x)

  if (is.null(d))
    return(FALSE)

  else if (length(d)==1)
    return(FALSE)

  else
    return(TRUE)

}




