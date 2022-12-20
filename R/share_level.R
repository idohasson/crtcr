share_number <- function(.id, ...) {
  unique_count(.id)
}

share_mean <- function(.id, ...) {
  unique_mean(.id)
}

share_level <- function(x, ..., .share_func=share_number) {

  dfl <- df_list(id=x, ..., .name_repair = "minimal")
  # return(dfl)
  if (vec_size(dfl) == 0)

    return(NA)

  else if (vec_size(dfl) == 1)

    return(.share_func(dfl$id))

  tapply(dfl$id, dfl[-1], .share_func, default = 0)

}
