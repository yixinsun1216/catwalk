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

#' 
#' @rdname read_latex
read_latex <- function(latex_file, keep_sig_stars = FALSE, keep_commas = TRUE, 
  keep_parens = TRUE){
  latex <- readLines(latex_file)

  latex <- 
    latex %>%
      str_split("&") %>%
      map_df(function(x) as_tibble(t(x))) %>%
      rename(stats_name = 1) %>% 
      na.omit() %>%
      mutate_all(funs(str_replace_all(., "\\\\", ""))) %>%
      mutate_all(funs(if_else(. == "", NA_character_, .))) %>%
      mutate_all(funs(str_replace_all(., 'phantom\\{.*?\\}', ''))) %>% 
      mutate_all(funs(str_replace_all(., '[a-zA-Z]+\\{.*?\\}\\{.*?\\}', ''))) %>%
      mutate_all(funs(str_replace_all(., '[{}]', ''))) %>%
      mutate_all(funs(str_replace_all(., '\\$', ''))) %>% 
      mutate_all(funs(str_replace_all(., 'raggedrightarraybackslash', ''))) %>% 
      mutate_all(trimws) %>% 
      mutate(stats_name = ifelse(row_number()==1 & stats_name=='', 
        'stats_name', stats_name)) %>% 
      mutate(stats_name = ifelse(stats_name=='', NA, stats_name)) %>% 
      setNames(as.character(.[1,])) %>% 
      .[-1,]

  empties <- 
    latex %>% 
    mutate(row_num = row_number()) %>% 
    filter(is.na(.[,1])) %>% 
    pull(row_num)

  if (sum(empties %% 2)==0) { # if even
    fromLast = FALSE
  } else {
    fromLast = TRUE
  }

  latex <- 
    latex %>% 
    na.locf(fromLast = fromLast)

  if (!keep_sig_stars) {
    latex <- 
      latex %>% 
      mutate_all(funs(str_replace_all(., '\\^[*].*$', '')))
  }
  if (!keep_commas) {
    latex <- 
      latex %>% 
      mutate_all(funs(str_replace_all(., ',', '')))
  } 
  if (!keep_parens) {
    latex <- 
      latex %>% 
      mutate_all(funs(str_replace_all(., '\\(', ''))) %>% 
      mutate_all(funs(str_replace_all(., '\\)', '')))
  }

  return (latex)
}

