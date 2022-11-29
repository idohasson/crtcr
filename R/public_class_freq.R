public_class_freq(c(1,1,0,0,0), c("A", "A", "B", "B", "C"))
public_class_freq(c(1,1,0,0,1), c("A", "A", "B", "B", "C"))
public_class_freq(c(1,0,0,0,0), c("A", "A", "B", "B", "C"))

public_class_freq(c(1,1,0,0,1))
public_class_freq(c(1,0,0,0,0))
public_class_freq(c(0,0,0,0,0))

#' Public classification of a clonotype
#'
#' @param .freq numerical vector
#' @param .subgroup identifier vector (optional)
#' @param .public value count threshold
#' @param .exclusive maximal number of subgroups a clone is found in.
#' @param na.rm if FALSE (the default), return NA when all frequency values are zero.
#'
#' @return character value ("private" / "public" / "exclusive” / “inclusive”)
#' @export
#'
#' @examples
#'
#' public_class_freq(c(1,1,0,0,0), c("A", "A", "B", "B", "C"))
#'
#' public_class_freq(c(1,1,0,0,0), c("A", "A", "B", "B", "C"), .public=.5)
#'
#' public_class_freq(c(1,1,0,0,1), c("A", "A", "B", "B", "C"), .exclusive=2)
#'
public_class_freq <- function(.freq, .subgroup=NULL, .public=2, .exclusive=1, na.rm=FALSE) {

  is_p <- is_public_freq(.freq, .public, na.rm)

  if (isFALSE(is_p))

    return("private")

  else if (is.na(is_p))

    return(NA)

  else if (is.null(.subgroup))

    return("public")

  is_e <- is_exclusive_freq(.freq, .subgroup, .exclusive, na.rm = TRUE)

  if (is_e)

    return("exclusive")

  else

    return("inclusive")

}
