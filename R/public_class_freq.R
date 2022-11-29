public_class_freq <- function(.freq, .subgroup, ...) {

  # PUBLIC_INDEX <- c("private", "inclusive", "exclusive")

  # tapply(.freq, .subgroup, is_public_freq, ...)

  is_p <- is_public_freq(.freq, ...)

  # is_e <- is_exclusive(.subgroup, ...)
  #
  # i <- (is_p) + (is_p & is_e)
  #
  # PUBLIC_INDEX[i+1]

}
