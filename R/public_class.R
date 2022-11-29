

#' Public classification
#'
#' returns the publicness classifications "private," "exclusive,” and
#' “inclusive” by the repertoires’ presence, throughout all subgroups.
#'
#'
#' @param .id repertoire identifier vector
#' @param ... additional identifier vectors.
#' @param .subgroup sub-group identification vector. for the default value, NULL, returns "public" instead of "exclusive” / “inclusive”
#' @param .public minimum number of repertoires.
#' @param .exclusive maximal number of subgroups a clone is found in.
#'
#' @return "private" / "exclusive” / “inclusive”
#' @export
#'
#' @examples
#'
#' public_class(c("A", "B"), .subgroup=c("C", "D"))
#' public_class(c("A", "B"), .subgroup=c("C", "C"))
#' public_class(c("A", "A"), .subgroup=c("C", "C"))
#'
#' public_class(c("A", "B"))
#' public_class(c("A", "A"))
#'
public_class <- function(.id, ..., .subgroup=NULL, .public=2, .exclusive=1) {

  is_p <- is_public(.id, ..., .public = .public)

  if (isFALSE(is_p))

    return("private")

  else if (is.null(.subgroup))

    return("public")

  is_e <- is_exclusive(.subgroup, .exclusive, .is_public = is_p, na.rm = TRUE)

  if (is_e)

    return("exclusive")

  else

    return("inclusive")

}
