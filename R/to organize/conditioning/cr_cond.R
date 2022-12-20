cr_cond <- function(nt, ..., condition=cr_prop(nt)>0) {
  # sum(as.logical(nt), na.rm = TRUE)>1
  condition_call <- substitute(condition)

  val <- list(nt, ...)

  eval(condition_call, val)

  # val <- do.call(func, l)
  # val
}
