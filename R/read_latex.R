#' @title Read Latex Table into R
#'
#' @description This function reads in latex code of a regression summary into 
#' R, returning the latex table in its entirety in a tibble format. 
#'
#' @param latex_file The path of the latex table to be converted into a tibble.
#' @param keep_sig_stars A Boolean specifying whether to keep significance stars
#' in the table.
#' @param keep_commas A Boolean specifying whether to keep commas in the table.
#' @param keep_parens A Boolean specifying whether to keep parentheses in the 
#' table.
#' @param keep_empties A Boolean specifying whether to keep empty 
#' variable names in the table (as opposed to backward-filling or 
#' forward-filling them). 
#' @return A tibble. 
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
#' # evaluate models
#' m1 <- felm(y ~ x1 + x2 | fe)
#' m2 <- glm(y ~ x1 + x2)
#' 
#' # format models using regtable
#' latex <- 
#'    regtable(list(m1, m2), est = list("x1", c("x1", "x2")), 
#'    stats = list(c("adj.r.squared"), c("AIC")),
#'    stats_names = list(c("$Adj R^2$"), c("AIC")), 
#'    sig_stars = TRUE, output_format = "latex")
#' 
#' # write regtable to latex
#' writeLines(latex, 'table.tex')
#' 
#' # read in latex regtable as tibble
#' table <- read_latex('table.tex') 
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom stringr str_split 
#' @importFrom zoo na.locf
#' @name read_latex
NULL

#===========
# read latex
#===========

library(zoo)
library(tidyverse)

empty_as_na <- function(x){
    if("factor" %in% class(x)) x <- as.character(x) 
    ifelse(as.character(x)!="", x, NA)
}

#' @rdname read_latex
read_latex <- function(latex_file, keep_sig_stars = TRUE, keep_commas = TRUE, 
  keep_parens = TRUE, keep_empties = FALSE){

  latex <- readLines(latex_file)

  # extract multicolumn header names (if they exist)
  multicolumn_header <- 
    latex[str_detect(latex, 'multicolumn')] %>% 
    str_split("&") %>%
    map_df(function(x) as_tibble(t(x))) %>% 
    mutate_all(funs(str_replace_all(., 
      '\\\\[a-zA-Z]+\\{.*?\\}\\{.*?\\}', ''))) %>% 
    mutate_all(funs(str_replace_all(., '\\{', ''))) %>% 
    mutate_all(funs(str_replace_all(., '\\}', ''))) %>% 
    mutate_all(funs(str_replace_all(., '\\\\', ''))) %>% 
    mutate_all(funs(str_replace_all(., 'textit', ''))) %>% 
    mutate_all(trimws) %>% 
    .[1,] %>% 
    as.character() %>% 
    .[.!='']

  # split latex by ampersand
  latex <- 
    latex %>%
      str_split("&")

  # put latex into tibble format, suppress the .name_repair argument warning.
  # to suppress this warning the pipeline unfortunately has to be broken out of
  latex <- suppressWarnings(map_df(latex, function(x) as_tibble(t(x))))

  # clean up names and special characters
  latex <- 
    latex  %>%
      rename(stats_name = 1) %>% 
      na.omit() %>%
      mutate_all(funs(str_replace_all(., "\\\\", ""))) %>%
      mutate_all(funs(if_else(. == "", NA_character_, .))) %>%
      mutate_all(funs(str_replace_all(., 'phantom\\{.*?\\}', ''))) %>% 
      mutate_all(funs(str_replace_all(., 
        '[a-zA-Z]+\\{.*?\\}\\{.*?\\}', ''))) %>%
      mutate_all(funs(str_replace_all(., '[{}]', ''))) %>%
      mutate_all(funs(str_replace_all(., '\\$', ''))) %>% 
      mutate_all(funs(str_replace_all(., 
        'raggedrightarraybackslash', ''))) %>% 
      mutate_all(funs(str_replace_all(., 'textit', ''))) %>% 
      mutate_all(funs(str_replace_all(., 'hline', ''))) %>% 
      mutate_all(trimws) %>% 
      mutate_all(funs(str_replace_all(., '\\[-1.8ex\\]', '')))

  # remove the redundant stargazer multicolumn header
  if (sum(str_detect(latex[1,], 'Dependent variable:'))>0) {
    latex <- latex[-1,]
  }

  # set first column name to stats_name if it doesn't exist otherwise
  latex <- 
    latex %>%
    mutate(stats_name = ifelse(row_number()==1 & stats_name=='', 
      'stats_name', stats_name)) %>% 
    mutate_all(funs(empty_as_na)) %>% 
    setNames(as.character(.[1,])) %>% 
    .[-1,]

  # group together multiple headers, if the first set of headers are needed to 
  # distinguish between duplicate names in the second set of headers  
  for (name in colnames(latex)) {
    indices <- which(name == colnames(latex)) 
    if (length(indices)>1) {
      if (length(multicolumn_header)>0) {
        for (i in 1:length(multicolumn_header)) {
          colnames(latex)[indices[i]] = paste(colnames(latex)[indices[i]], 
            multicolumn_header[i], sep = ' - ')
        }
      } else {
        for (i in 1:length(indices)) {
          colnames(latex)[indices[i]] = paste(colnames(latex)[indices[i]], 
            i, sep = ' - ')
        }
      }
    }
  }

  # remove entirely null rows 
  latex <- 
    latex %>%   
    filter_all(any_vars(!is.na(.)))

  # optionally rename empty variable names to those immediately 
  # preceding or proceeding them (preceding if the empties are in an odd order)
  if (!keep_empties) {
    empties <- 
      latex %>% 
      mutate(row_num = row_number()) %>% 
      filter(is.na(.[,1])) %>% 
      pull(row_num)

    if (empties[1]>2) {
      empties <- empties - (empties[1] - 2)
    }

    if (sum(empties %% 2)==0) { # if even
      fromLast = FALSE
    } else {
      fromLast = TRUE
    }

    latex[1] <- 
      latex[1] %>% 
      na.locf(fromLast = fromLast)
  }

  # optionally remove significance stars
  if (!keep_sig_stars) {
    latex <- 
      latex %>% 
      mutate_all(funs(str_replace_all(., '\\^[*].*$', ''))) %>% 
      mutate_all(funs(str_replace_all(., 'sym|\\*', '')))
  }
  # optionally remove commas
  if (!keep_commas) {
    latex <- 
      latex %>% 
      mutate_all(funs(str_replace_all(., ',', '')))
  } 
  # optionally remove parentheses
  if (!keep_parens) {
    latex <- 
      latex %>% 
      mutate_all(funs(str_replace_all(., '\\(', ''))) %>% 
      mutate_all(funs(str_replace_all(., '\\)', '')))
  }

  return (latex)
}

extract_column <- function(col, stats) {
  col_estimate <- 
      stats %>% 
      pull(col) %>% 
      str_replace('\\(', '') %>% 
      str_replace('\\)', '') %>% 
      str_replace(',', '') %>% 
      str_replace_all(., '\\^[*].*$', '') %>% 
      str_replace_all(., '\\*', '') %>% 
      as.double()
  return (col_estimate)
}

pull_numbers <- function(df, variable, cols)  {
  stats <- 
    df %>% 
    filter_at(1, all_vars(. == variable)) %>% 
    .[-1]

  if (is.null(cols)) {cols <- seq(1, dim(stats)[2])}
  
  estimates <- map(cols, extract_column, stats)

  return (estimates)
}