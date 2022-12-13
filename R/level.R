library(vctrs)
library(rlang)


# values - numerical values
# intervals - numerical integer / funciton or formula to apply on values to use as interval
level <- function(values, intervals=median) {
  
  if (is_formula(intervals))
    
    intervals <- as_function(intervals)
  
  if (is_function(intervals))
    
    intervals <- intervals(values)
  
  findInterval(values, intervals)
  
}

top_prec <- function(values, top=c(.25, .5, .75), rm.dup=TRUE) {
  
  value_count <- table(values)
  
  count_prop <- proportions(value_count)
  
  cum_prop <- cumsum(count_prop)
  
  first_higher <- function(x) {
    
    i <- which.max(cum_prop >= x)
  
    as.numeric(names(i))
  
  }
  
  levels <- vapply(top, first_higher, numeric(1))
  
  if (isTRUE(rm.dup)) 

    levels <- levels[!duplicated(levels)]
  
  
  levels

}

cr_func <- function(x) {
  
  cr_seq <- split(x, translate(x))
  
  vapply(cr_seq, vec_unique_count, numeric(1))
  
}


cr_values <- cr_func(NT)

level(cr_values, 2)
