same_unique_aa <- function(nt) {

  if (length(nt)==1)

    return(TRUE)

  aa <- translate(nt)

  length(unique(aa)) == 1

}
