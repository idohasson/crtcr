library(readr)
library(dplyr)
library(tools)
library(purrr)


check_fields <- function(file_path, fields_name) {
  has_fields <- fields_name %in% read.table(file = file_path, header = F,nrows = 1)
  return(fields_name[has_fields])
}

filter_paths <- function(file_paths, fields_name) {
  has_col <- sapply(file_paths, function(p) all(fields_name %in%  read.table(file = p,header = F,nrows = 1)))
  return(names(has_col)[has_col])
}

files_from_dir <- function(dir_path, with_col_names) {
  file_paths <- list.files(dir_path, full.names=TRUE)
  paths_with_fields <- filter_paths(file_paths, with_col_names)
  return(paths_with_fields)
}

read_by_col_names <- function(read_paths, col_names) {
  
  # paths_with_fields <- files_from_dir(paths, col_names)
  
  cols_list <- lapply(read_paths, read_tsv, col_select = all_of(col_names), show_col_types = FALSE)
  
  return(cols_list)
}



get_path_group <- function(p, g) lapply(p, get_group, groups=g)
# dirs <- list(DIR_ALPHA, DIR_BETA)

groups_in_dir <- function(dir_path, read_col, file_name_contain, group_name) {
  paths <- dir_path %>% 
    files_from_dir(with_col_names=read_col) %>% 
    lapply(X = file_name_contain, FUN = grep, value = TRUE) %>% 
    setNames(group_name)
  return(paths)
}

files_with_pattern <- function(pattern, file_paths) {
  with_pattern <- file_paths %>% 
    basename %>% 
    file_path_sans_ext %>% 
    grep(pattern = pattern)
  return(file_paths[with_pattern])
}

from_dir <- function(dir_path, read_col, file_name_contain) {
  file_paths <- files_from_dir(dir_path, read_col)
  file_paths <- file_name_contain %>% 
    lapply(FUN = files_with_pattern, file_paths)
  return(file_paths)
}

read_paths <- function(path_vec, fields) {
  path_vec %>% 
    setNames(file_path_sans_ext(basename(.))) %>%
    lapply(read_tsv, col_select = all_of(fields), show_col_types = FALSE) %>% 
    returnValue()
}


list_to_df <- function(df_list) {
  
  df <- flatten_dfr(df_list)
  
  df['sample'] <- df_list %>% 
    lapply(function(l) returnValue(lapply(l, nrow))) %>% 
    unlist %>%  
    rep(x = names(.))
  
  return(df)
  
}

read_paths_from_dir <- function(path_list, fields) {
  path_list %>% 
    lapply(read_paths, fields = fields) %>% 
    list_to_df
}


