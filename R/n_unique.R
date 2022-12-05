n_unique <- function(.clone, na.rm = TRUE) {

  if (isTRUE(na.rm)) {

    no_na <- vec_detect_complete(.clone)

    .clone <- vec_slice(.clone, no_na)

  }

  vec_unique_count(.clone)

}
