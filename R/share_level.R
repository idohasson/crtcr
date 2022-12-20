share_number <- function(.id, ...) {
  unique_count(.id)
}

share_mean <- function(.id, ...) {
  unique_mean(.id)
}

# share_level
# share_level(nt, aa, id)
share_level <- function(..., .share_func=share_number) {

  dfl <- df_list(..., .name_repair = "minimal")

  if (vec_size(dfl) == 0)

    return(NA)

  else if (vec_size(dfl) == 1)

    return(.share_func(dfl[[1]]))

  tapply(dfl[[1]], dfl[-1], .share_func)

}

