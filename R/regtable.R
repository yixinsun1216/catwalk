#' @title Format R Regression Output 
#'
#' @description This routine creates latex code, HTML code, and text tables that
#' present regression output.
#'
#' @param ms A list of one or more model objects that are 
#'    compatible with the summary() function.
#' @param est A character vector of the covariates to output. To output 
#'    different coefficients for different models, pass in est as a list, where
#'    est[[i]] specifies the covariates to output for ms[[i]]. 
#' @param est_names A character vector of labels for est. If outputting
#'    different covariates for different models, pass in est_names as a list, 
#'    where est_names[[i]] specifies the names for est[[i]]. 
#' @param mnames A character vector of labels for each model object in ms
#' @param extra_rows A character vector additional information to display for
#'    each model, such as fixed effects or controls.
#' @param stats A character vector specifying which model statistics should be
#'    kept in the output. The names of the statistics should match variable names
#'    created by \code{\link[broom:glance]{glance}} (r.squared, adj.r.squared, 
#'    sigma, p.value). To output different summary statistics for different 
#'    models, create a list of length(ms), where stats[[i]] specifies the 
#'    summary statistics for ms[[i]]. 
#' @param stats_names A character vector of labels for each object in stats. If 
#'    outputting different summary statistics for different models, pass in
#'    stats_names as a list, where stats_names[[i]] specifies the names for
#'    stats[[i]].
#' @param output_format A string passed to kable() that specifies the format of
#'    the table output. The options are "latex", "html", "markdown", "pandoc", 
#'    and "rst". The default is "latex". Additionally, passing in "df" will output a 
#'    dataframe version of the table, which can be used with regtable_stack()
#'    to create a table with multiple regression summaries. 
#' @param sig_stars Logical indicating whether or not significant stars should
#'    be added to the coefficients.
#' @param note A character string if a footnote is to be added to the end of the
#'    table.
#' @param header A character vector to be passed into 
#'    \code{\link[kableExtra:add_header_above]{add_header_above}} that creates
#'    a new header row. This should have length equal to ms from regtable(). 
#' 
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
#' regtable(list(m1, m2), est = list("x1", c("x1", "x2")), 
#'          stats = list(c("adj.r.squared"), c("AIC")),
#'          stats_names = list(c("$Adj R^2$"), c("AIC")), 
#'          sig_stars = TRUE, output_format = "rst")

#' @importFrom magrittr %>%
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_dfc map2_df reduce map2
#' @importFrom stats symnum
#' @importFrom knitr kable
#' @importFrom kableExtra collapse_rows row_spec add_footnote
#' @importFrom stringr str_replace_all
#' @importFrom tidyr gather
#' @importFrom rlang quo_name enquo
#' @import dplyr
#' @importFrom broom glance
#' @importFrom lmtest coeftest
#' @name regtable
NULL


# function for creating significant stars
significance <- function(x){
  symp <- symnum(x, corr = FALSE, na = FALSE,
                 cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                 symbols = c("$^{***}$", "$^{**}$", "$^{*}$", "$^{}$"))
  return(as.character(c(symp)))
}

# Extracting coefficients and standard errors -------------------------------
get_tau <- function(m, colname, se_fun, decimals, est, est_names,
                    sig_stars, ...) {
  if(is.null(est_names)) est_names <- est
  est_names_map <- tibble(term = est, term_name = est_names)

  if(is.null(decimals)) decimals <- 3

  colname1 <- quo_name(enquo(colname))

  # extract coefficients and standard errors
  output <- coeftest(m, se_fun(m, ...))
  
  # add significant stars if sig_stars == TRUE
  output <-
    output %>%
    tidy() %>%
    filter(term %in% est) %>%
    mutate(sigs = sig_stars,
           stars = if_else(sigs, significance(`p.value`), ""),
           estimate = trimws(format(round(estimate, decimals),
                                    nsmall = decimals)),
           estimate = paste0(estimate, stars))

  # tack on dummy rows for values of est that aren't in term
  missing_est <- setdiff(est, with(output, term))
  output_extra <- tibble(term = missing_est,
                         estimate = NA_real_,
                         std.error = NA_real_,
                         statistic = NA_real_,
                         p.value = NA_real_)
  output <- bind_rows(output, output_extra)

  # format values such that it's rounded to 3 decimal places
  # format value so output has SE and coefficients lined up and centered
  output %>%
    select(term, estimate, se = `std.error`) %>%
    mutate(se = trimws(format(round(se, decimals), nsmall = decimals))) %>%
    inner_join(est_names_map, by = "term") %>%
    select(-term) %>%
    rename(term = term_name) %>%
    mutate(index = row_number()) %>%
    gather(type, value, -term, -index) %>%
    group_by(index) %>%
    arrange(index, type) %>%
    ungroup %>%
    select(-index) %>%
    mutate(sigs = sig_stars,
           value = if_else(type == "se", paste0("(", value, ")"),
                           as.character(value)),
           value = format(value, justify = "centre")) %>%
    rename(!!colname1 := value) %>%
    select(-sigs)

}


# Extract summary statistics from model --------------------------------------
get_stats <- function(m, stat1, stats_name, mnames, n_obs){
  stats_out <- glance(m)

  if(!is.null(stat1)) stats_out <- select(stats_out, one_of(stat1))
  if(is.null(stats_name)) stats_name <- colnames(stats_out)
  
  stats_out <-
    stats_out %>%
    t() %>%
    round(., 3) %>%
    format(., nsmall = 3, big.mark = ",") %>%
    str_replace_all(., ".000$", "") %>%
    trimws

  if(n_obs){
    stats_out <- c(format(length(fitted(m)), big.mark = ","), stats_out) 
    stats_out <- bind_cols(!!mnames := stats_out, term = c("N", stats_name))
  } else{
    stats_out <- bind_cols(!!mnames := stats_out, term = stats_name)
  }
 
  return(as_tibble(stats_out))
}

# Main regtable function ------------------------------------------------------
#' @export
#' @rdname regtable
regtable <- function(ms, est, mnames = NULL, est_names = NULL,
                     extra_rows = NULL, se_fun = vcov,
                     stats = c("r.squared", "adj.r.squared"),
                     stats_names = c("$R^2$", "Proj. $R^2$"),
                     n_obs = TRUE,
                     output_format = "latex",
                     header = NULL,
                     sig_stars = FALSE,
                     decimals = NULL,
                     note = NULL, ...) {

  temp_names <- paste("(", seq(1:length(ms)), ")")
  if(is.null(mnames)) mnames <- temp_names
  if(is.null(decimals)) decimals <- rep(3, length(ms))

  # create section of table housing stats such as N, R^2 and Projected R^2
  if(!is.list(stats)){ stats <- list(stats)}
  if(!is.list(stats_names)){ stats_names <- list(stats_names)}
  if(!is.list(n_obs)){n_obs <- list(n_obs)}

  statstable <-
    pmap(list(ms, stats, stats_names, mnames, n_obs), get_stats) %>%
    reduce(full_join, by = "term") %>%
    select(term, everything()) %>%
    mutate(type = "")

  # create section of table housing coefficients and standard error
  if(!is.list(se_fun)) se_fun <- list(se_fun)
  if(!is.list(est)) est <- list(est)
  if(!is.list(est_names)) est_names <- list(est_names)

  coef_table <-
    pmap(list(ms, mnames, se_fun, decimals, est, est_names),
         get_tau, sig_stars, ...) %>%
    reduce(full_join, by = c("term", "type"))

  # replace NA and (NA) with spaces in Latex Tables
  if(output_format == "latex") {
    coef_table <-
      coef_table %>%
      mutate_at(vars(mnames),
                funs(str_replace(., regex("NA|\\(NA\\)"), "\\phantom{X}")))
  }

  # add in extra rows
  if(!is.null(extra_rows)) {
    extras <-
      extra_rows %>%
      unlist() %>%
      matrix(ncol = length(extra_rows)) %>%
      t() %>%
      as_tibble() %>%
      rename_all(~ mnames) %>%
      mutate(type = "",
             term = names(extra_rows))
    row_spec_no <- length(extra_rows) + 2 * max(map_dbl(est, length))

  } else {
    extras <- NULL
    row_spec_no <- NA_integer_
  }

  # bind together the regression coefficients, stats, and extra lines
 if(output_format == "df"){
    final_table <-
      mutate(coef_table, part = "coef") %>%
      bind_rows(mutate(statstable, part = "stats")) 

    if(!is.null(extras)){
      final_tables <-
        final_tables %>%
        bind_rows(mutate(extras, part = "extra"))
    }
    return(list(output = final_table, model_names = mnames)) 
  }

  final_table <-
    bind_rows(coef_table, extras, statstable) %>%
    mutate_all(funs(if_else(is.na(.), "", .))) %>%
    select(-type) %>%
    rename(` ` = term)  %>%
    kable(format = output_format,
          booktabs = TRUE,
          col.names = c("", mnames),
          linesep = "",
          escape = FALSE,
          align = c('l', rep('c', length(mnames)))) %>%
    add_footnote(note)

  if(!is.null(header)) final_table <- final_table %>% add_header_above(header)

  if(output_format == "latex"){
    final_table <-
      final_table %>%
      row_spec(2 * max(map_dbl(est, length)), 
        extra_latex_after = "\\midrule") %>%
      row_spec(row_spec_no, extra_latex_after = "\\midrule") %>%
      collapse_rows(columns = 1, latex_hline = "none")

    # ridiculous hack to get latex \ back in phantoms
    final_table <-
      final_table %>%
      str_replace_all(fixed("phantom{X}"), "\\phantom{X}")
  }

  return(final_table)
}

