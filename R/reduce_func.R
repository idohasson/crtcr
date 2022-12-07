condition_func <- function(x, y, op="+") vec_arith(op,x, y)

redce_freq_func <- function(v, op=">", count_n=0) {
  
  
  
} 


redce_char_func <- function(v, ...) {
  
  freq <- vec_count(v)$count
  
  redce_freq_func(freq, ...)
  
} 
  
  vec_arith(op="<=",x, y=2)

condition2_func <- function(x, y, op="<=") vec_arith(op="<=",x, y=2)

reduce_cond <- function(v, n=2, cond=condition2_func) {
  
  Reduce(condition_func, as.logical(v))

}

Reduce(condition_func, as.logical(c(0,1,3)))


Reduce(condition_func, x = c(0,1,3), accumulate = F)

vec_count(c("a","b", "a", "c"))$count