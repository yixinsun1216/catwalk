#' @title Read latex output into R
#'
#' @description This function reads in Latex code of a regression summary into R
#'
#' @param latex_output The latex code to be converted into R objects
#' @param output A character vector specifying which type of regression output 
#' should be converted to an R object. Options are "coef", "stats", and "extra". 
#' "coef" refers to regression coefficients and standard errors, and "stats" 
#' refers to other statistics such as N and R^2. "extra" should be used if the 
#' latex code you want to read into R was created by regtable() using the 
#' extra_rows argument, and you're interested in seeing the content of 
#' those extra rows. 

#' @examples
#' library(lfe)
#' 
#' # create covariates
#' x1 <- rnorm(1000)
#' x2 <- rnorm(length(x1))
#' 
#' # fixed effects
#' fe <- factor(sample(20, length(x1), replace=TRUE))
#' 
#' # effects for fe
#' fe_effs <- rnorm(nlevels(fe))
#' 
#' # creating left hand side y
#' u <- rnorm(length(x1))
#' y <- 2 * x1 + x2 + fe_effs[fe] + u
#' 
#' m1 <- felm(y ~ x1 + x2 | fe)
#' m2 <- glm(y ~ x1 + x2)
#' 
#' 
#' latex_output <- 
#'    regtable(list(m1, m2), est = list("x1", c("x1", "x2")), 
#'    stats = list(c("adj.r.squared"), c("AIC")),
#'    stats_names = list(c("$Adj R^2$"), c("AIC")), 
#'    sig_stars = TRUE, output_format = "latex")
#' 
#' latex_coef <- read_latex(latex_output, output = 'coef')
#' latex_stats <- read_latex(latex_output, output = 'stats')
#' 
#' @import tidyverse
#' @import knitr
#' @import readr
#' @import janitor
#' @name read_latex
NULL

library(tidyverse)
library(knitr)
library(readr)
library(janitor)

#===========
# read latex
#===========
split_vec <- function(vec, sep = 0) {
      is.sep <- vec == sep
      split(vec[!is.sep], cumsum(is.sep)[!is.sep])
}

#' @export
#' @rdname read_latex
read_latex <- function(latex_output, output = 'coef') {
  if(sum(str_detect(latex_output, "\n")) > 0){
    latex_output <- 
      latex_output %>%
      paste(collapse = "") %>%
      str_split("\n") %>%
      pluck(1) 
  } 

  latex_split <- 
    latex_output %>%
    str_replace_all("phantom\\{X\\}", "") %>%
    .[. != ""] %>%
    split_vec("\\midrule")

  # IDEA: split up the latex output into the 4 main components:
  # 1. Header
  # 2. Coefficients/se
  # 3. Extra lines
  # 4. summary stats
  # split into sections by "midrule"

  header <- latex_split[[1]]
  coef <- latex_split[[2]]

  if(length(latex_split) == 3){
    stats <- latex_split[[3]]
  } else if(length(latex_split) == 4){
    stats <- latex_split[[4]]
    extra <- latex_split[[3]] %>% 
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
    mutate_all(funs(str_replace_all(., "\\\\", ""))) %>% 
    mutate_all(funs(str_replace_all(., ",", "")))

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
    mutate_all(funs(str_replace_all(., "\\$\\^\\{.*?\\}\\$", ""))) %>%
    mutate_all(funs(if_else(. == "", NA_character_, .))) %>%
    mutate(type = if_else(is.na(est_name), "coef", "se")) %>%
    fill(est_name, .direction = "up") %>%
    remove_empty('cols')

  if (output=='coef') {
    return (coef)
  } else if (output=='stats') {
    return (stats)
  } else if (output=='extra') {
    return (extra)
  } else {
    stop("invalid output choice (choose either 'coef', 'stats', or 'extra')")
  }
}