library(tidyverse)
library(knitr)
library(readr)

tdir <- "C:/Users/Yixin Sun/Documents/Github/texas/output/tables"

#===========
# read latex
#===========

latex_file <- file.path(tdir, 'bonus_regressions.tex')
latex <- readLines(latex_file) %>% .[. != ""]

#===========
# do stuff
#===========

# IDEA: split up the latex output into the 4 main components:
# 1. Header
# 2. Coefficients/se
# 3. Extra lines
# 4. summary stats
# split into sections by "midrule"

split_vec <- function(vec, sep = 0) {
    is.sep <- vec == sep
    split(vec[!is.sep], cumsum(is.sep)[!is.sep])
}

latex_reader <- function(latex){
  if(sum(str_detect(latex, "\n")) > 0){
    latex_split <- 
      latex %>%
      paste(collapse = "") %>%
      str_split("\n") %>%
      pluck(1) %>%
      .[. != ""] %>%
      split_vec("\\midrule")
  } else{
    latex_split <- split_vec(latex, sep = "\\midrule")
  }
  
  header <- latex_split[[1]]
  coef <- latex_split[[2]]

  if(length(latex_split) == 3){
    stats <- latex_split[[3]]
    extra <- as_tibble()

  } else if(length(latex_split) == 4){
    stats <- latex_split[[4]]
    extra <- latex_split[[3]]

    extra <-
      extra %>%
      str_split("&") %>%
      map_df(function(x) as_tibble(t(x))) %>%
      rename(extra_name = 1) %>%
      filter(!str_detect(extra_name, "\\\\")) %>%
      mutate_all(trimws) %>%
      mutate_all(funs(str_replace_all(., "\\\\", "")))
  }

  # stats and extra can be made into dataframes
  stats <- 
    stats %>%
    str_split("&") %>%
    map_df(function(x) as_tibble(t(x))) %>%
    rename(stats_name = 1) %>%
    filter(!str_detect(stats_name, "\\\\")) %>%
    mutate_all(trimws) %>%
    mutate_all(funs(str_replace_all(., "\\\\", "")))

  # coefficients
  coef <-
    coef %>%
    str_split("&") %>%
    map_df(function(x) as_tibble(t(x))) %>%
    rename(est_name = 1) %>%
    mutate(est_name = str_extract(est_name, "(\\s.*?)(\\})"), 
      est_name = str_replace(est_name, "\\}", "")) %>%
    mutate_all(trimws) %>%
    mutate_all(funs(str_replace_all(., "\\\\", ""))) %>%
    mutate(type = if_else(is.na(est_name), "coef", "se")) %>%
    fill(est_name, .direction = "up") 

  header <-
    header[str_detect(header, "&")] %>%
    str_split("&") %>%
    pluck(1) %>%
    trimws() %>%
    str_replace_all("\\\\", "") %>%
    .[. != ""]

  output <- list(coef = coef, stats = stats, extra = extra, header = header)

  return(output)

}
