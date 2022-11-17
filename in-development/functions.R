
get_clone_attr <- function(l, clone_attr) map(l, clone_attr)

flat_clonotypes_chr <- function(l) flatten_chr(map(l, unique))

flat_clonotypes_df <- function(df) flatten_df(map(df, unique))
cr_level_nt <- function(nt) tapply(nt, translate(nt), n_unique_f)
cr_level_nt <- function(nt) tapply(nt, translate(nt), mean_unique_f)



# function or lambda formula to apply on 
# each sub set of NT sequences encoding
# a specific AA sequence. 
# 
# tapply(nt_vec, translate(nt_vec), func)
# 
# Example:
# 
# 
# 

n_unique_f <- ~ length(unique(.x))

func <- rlang::as_function(n_unique_f)

map_func <- as_mapper(~ tapply(.x, translate(.x), func))

map(get_clone_attr(dfl[1:3], nt), map_func)



v <- get_clone_attr(dfl[1], nt)[[1]]

# total number of NT sequences encoding a specific AA sequence
f1 <- nt_vec_f(length)
f1(v)

# average number of unique NT sequences encoding a specific AA sequence 
f2 <- nt_vec_f(~mean(table(.x)))
f2(v)

nt_vec_f <- function(lambda_func) {
  
  func <- rlang::as_function(lambda_func)
  
  rlang::as_function(~ tapply(.x, translate(.x), func))
  
}

l <- get_clone_attr(dfl[1:3], nt)
# total number of NT sequences encoding a specific AA sequence
f3 <- nt_vec_map_f(~length(unique(.x)))
map(l, f3)


# average number of unique NT sequences encoding a specific AA sequence 
my_cool_func <- function(x) max(table(x))
f4 <- nt_vec_map_f(my_cool_func)
map(l, f4)

nt_vec_map_f <- function(lambda_func) {
  
  v_func <- nt_vec_f(lambda_func)
  
  as_mapper(v_func)
  
}




nt_vec_f(length)(get_clone_attr(dfl[[1]], nt))

map(get_clone_attr(dfl[1:3], nt), nt_list_f(length))



n_unique_f <- ~ length(unique(.x))
# n_unique_f <- as_mapper(~ length(unique(.x)))

mean_unique_f <- as_mapper(~ mean(table(.x)))







get_clone_attr(dfl, nt)
  
  flat_clonotypes_chr()
  

f_new <- nt_f(n_unique_f)

get_clone_attr(dfl[1:3], nt)
  
  # map(as_mapper(~ tapply(.x, translate(.x), rlang::as_function(n_unique_f))))

# map(as_mapper(~ f_new(.x)))
  
# tapply(., translate(.), length)


  


# as_mapper(~ length(unique(.x)))






# dfl[1:3] %>% map(as.list) %>% map_chr(as_mapper(~ aa))
  
  


is_public <- function(clonotype_attr) {
  map_chr(clonotype_attr, ~anyDuplicated.data.frame(.x))
}
anyDuplicated.data.frame()

paths <- list.files("~/R/R Programming/datasetsR/beta_MiXCR/", full.names = TRUE)
dfl <- map(paths, read.delim, nrows = 10)

nt_list <- get_clone_attr(dfl[1:3], "nSeqCDR3")

nt <- "nSeqCDR3"; aa <- "aaSeqCDR3"; id <- "r_id"

func <- as_mapper(~ translate(unique(.x)))

nt_list %>% 
  
  map(func) %>% 
  
  flatten_chr() %>% 
  
  .[duplicated(.)]


# func <- as_mapper(~ length(unique(.x)))

# map(nt_list, func)
# map_chr(nt_list[[1]], translate)

# map_chr()
# translate()


# tapply(attr1[nt], attr1[c(id, aa)], unique)