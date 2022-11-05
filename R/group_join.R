#' Join all repertoires' clonotypes data into one data frame
#'
#' @param ... repertoire group
#'
#' @return data frame
#'
#' @importFrom rlang list2
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr modify_depth map pluck
#' @importFrom magrittr %<>% %>%
#'
#' @export
#'
#' @examples
#'
#' nt_gen <- clone_gen()
#'
#' group1 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#' group2 <- list(sapply(rpois(100, 2)+1, nt_gen), sapply(rpois(100, 2)+1, nt_gen))
#'
#' group_join(list(a=group1, b=group2))
#'
#'
group_join <- function(...) {

  rep <- list2(...) # TODO: dots_list
  # TODO check valid input
  # TODO make sure rep values are unique valid input
  if (is.vector(rep)) {

    if (is.vector(pluck(rep, 1))) {

      if (is.list(rep) & length(rep)==1)
        rep %<>% pluck(1)

      if (is.character(pluck(rep, 1, 1)))
        rep %<>% modify_depth(2, function(nt)
          cbind.data.frame(clone=nt,
                           clonotype=translate(nt)))

      if (is.data.frame(pluck(rep, 1, 1)))
        rep %<>% map(bind_rows, .id = "rep_id")
    }

    if(is.data.frame(pluck(rep, 1)))
      rep %<>% bind_rows(.id = "group")

  } else if (is.data.frame(rep)) {

    if (is.character(rep[1,3]) & ncol(rep)==3)
      rep %<>% mutate(clonotype=translate(pluck(3)))

  } else stop("Invalid input error")

  # TODO check valid input
  if (ncol(rep) == 4)
    colnames(rep) <- c("group", "rep_id", "clone", "clonotype")
  else stop("ERROR")

 rep

}
