unique_aa <- function(nt, aa=translate(nt[1])) {

  if (length(nt)==1)

    return(TRUE)

  for (nt_seq in nt[-1])

    if (aa != translate(nt_seq))

      return(FALSE)

  return(TRUE)

}
