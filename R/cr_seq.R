cr_seq <- function(nt, aa) {
  
  cr_map <- fastmap()
  
  do.call(cr_map$mset, split(nt, aa))
  
  return(cr_map)
  
}
