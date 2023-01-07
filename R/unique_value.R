cr_list <- with(x, list(
  n=unique_value(split(nt, aa), freq = FALSE, inverse = FALSE),
  freq=unique_value(split(nt, aa), freq = TRUE, inverse = FALSE),
  inv=unique_value(split(nt, aa), freq = FALSE, inverse = TRUE),
  inv.freq=unique_value(split(nt, aa), freq = TRUE, inverse = TRUE)
))

# count_unique(rand_nt_vec(30))
count_unique <- function(x) {
  # TODO use vapply to replace `unique_value` so it could be applied on group
  vec_unique_count(x)
}

# freq_unique(rand_nt_vec(30))
freq_unique <- function(x) {
  vec_unique_count(x) / vec_size(x)
}

# inverse_unique(rand_nt_vec(30))
inverse_unique <- function(x) {
  1 / vec_unique_count(x)
}

# inverse_freq_unique(rand_nt_vec(30))
inverse_freq_unique <- function(x) {
  1 / (vec_unique_count(x) / vec_size(x))
}

# N=100
# unique_value(rand_nt_vec(N), freq = F, inverse = F)
# unique_value(rand_nt_vec(N), freq = T, inverse = F)
# unique_value(rand_nt_vec(N), freq = F, inverse = T)
# unique_value(rand_nt_vec(N), freq = T, inverse = T)
# nr_ve_list <- split(rand_nt_vec(N), sample(10, N, TRUE))
# unique_value(nr_ve_list, freq = F, inverse = F)
# unique_value(nr_ve_list, freq = T, inverse = F)
# unique_value(nr_ve_list, freq = F, inverse = T)
# unique_value(nr_ve_list, freq = T, inverse = T)
unique_value <- function(..., freq=FALSE, inverse=FALSE, named=FALSE) {
  
  if (isTRUE(freq) & isTRUE(inverse)) {
    
    unique_func <- inverse_freq_unique
    
  } else if (isTRUE(freq)) {
    
    unique_func <- freq_unique
    
  } else if (isTRUE(inverse)) {
    
    unique_func <- inverse_unique
    
  } else {
    
    unique_func <- count_unique
    
  }
  
  vapply(rlang::dots_splice(...), unique_func, numeric(1), USE.NAMES = named)
  
}

