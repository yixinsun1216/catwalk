#' @title Regression output from R regression output
#'
#' @description This routine creates Latex code, HTML code, and text tables for
#' output of a regression summary
#'
#' @param ms one or more model objects that are compatible with the summary()
#'    function
#' @param est a character vector of the covariates to output
#' @param est_names a character vector of labels for est
#' @param mnames a character vector of labels for each model object in ms
#' @param extra_rows a character vector additional information to display for
#'    each model, such as fixed effects or controls.
#' @param stats a character vector specifying which model statistics should be
#'    kept in the output
#' @param stats_names a character vector of labels for each object in stats
#' @param output_format A string passed to kable() that specifies the format of
#'    the table output. The options are latex, html, markdown, pandoc, and rst.
#'    The default is latex
#' @param sig_stars logical indicating whether or not significant stars should
#'    be added to the coefficients
#' @param note A character string if a footnote is to be added to the end of the
#'    table.
#'
#'
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
NULL 

# Main function
#' @export regtable regtable_stack

# function for creating significant stars
significance <- function(x){
  symp <- symnum(x, corr = FALSE, na = FALSE,
                 cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                 symbols = c("$^{***}$", "$^{**}$", "$^{*}$", "$^{}$"))
  return(as.character(c(symp)))
}

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
  if (is.null(se_fun)) {
    output <- coeftest(m)
  } else {
    output <- coeftest(m, se_fun(m, ...))
  }

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
get_stats <- function(m, stats, n_obs){
  stats_out <-
    glance(m) %>%
    select(one_of(stats)) %>%
    t() %>%
    round(., 3) %>%
    format(., nsmall = 3, big.mark = ",") %>%
    str_replace_all(., ".000$", "") %>%
    trimws

  if(n_obs) stats_out <- c(format(length(fitted(m)), big.mark = ","), stats_out)

  return(stats_out)
}

# Main regtable function ------------------------------------------------------
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
  statstable <-
    ms %>%
    map_dfc(function(x) get_stats(x, stats, n_obs)) %>%
    setNames(mnames) %>%
    mutate(type = "")

  if(n_obs){
    statstable <- mutate(statstable, term = c("N", stats_names))
  } else {statstable <- mutate(statstable, term = stats_names)}


  # create section of table housing coefficients and standard error
  if(!is.list(se_fun)) se_fun <- list(se_fun)

  coef_table <-
    pmap(list(ms, mnames, se_fun, decimals),
         get_tau, est, est_names, sig_stars, ...) %>%
    reduce(inner_join)

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
    row_spec_no <- length(extra_rows) + 2 * length(est)

  } else {
    extras <- NULL
    row_spec_no <- NA_integer_
  }

  # bind together the regression coefficients, stats, and extra lines
  if(output_format == "df"){
    final_table <-
      mutate(coef_table, part = "coef") %>%
      bind_rows(mutate(extras, part = "extra")) %>%
      bind_rows(mutate(statstable, part = "stats"))
    return(final_table)
  }

  final_table <-
    final_table <-
    bind_rows(coef_table, extras, statstable) %>%
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
      row_spec(2 * length(est), extra_latex_after = "\\midrule") %>%
      row_spec(row_spec_no, extra_latex_after = "\\midrule") %>%
      collapse_rows(columns = 1, latex_hline = "none")

    # ridiculous hack to get latex \ back in my phantoms...
    final_table <-
      final_table %>%
      str_replace_all(fixed("phantom{X}"), "\\phantom{X}")
  }

  return(final_table)
}


# regtable stack ------------------------------------------------------------
# function that takes several regtable outputs (in dataframe format) and
# stacks them together
regtable_stack <- function(final_tables, table_names = NULL, output_format = "latex",
  note = NULL, header = NULL){

  if(!is.null(table_names)){
    final_df <-
      map2_df(final_tables, table_names,
        function(x, y) mutate(x, table_name = y))
  } else{
    final_df <-
      map_df(final_tables, c) %>%
      mutate(table_name = NA)
  }

  final_df <- mutate(final_df, part = if_else(term == "N", "extra", part))

  coef <-
    final_df %>%
    filter(part == "coef" | part == "stats")  %>%
    mutate(term = if_else(part == "coef" & !is.na(table_name),
      paste(term, "-", table_name), term))

  extra <-
    final_df %>%
    filter(part == "extra") %>%
    select(-table_name) %>%
    distinct()

  output <-
    bind_rows(coef, extra) %>%
    select(-type, -table_name, -part) %>%
    rename(` ` = term)

  mnames <- colnames(output)

  output <-
    output %>%
    kable(format = output_format,
          booktabs = TRUE,
          col.names = colnames(.),
          linesep = "",
          escape = FALSE,
          align = c('l', rep('c', length(mnames)))) %>%
    add_footnote(note)

  if(!is.null(header)) final_table <- final_table %>% add_header_above(header)

  if(output_format == "latex"){
    break_end <- nrow(coef)
    break_start <- break_end / length(final_tables)
    breaks <- seq(break_start, break_end, break_start)

    output <-
      output %>%
      row_spec(breaks, extra_latex_after = "\\midrule") %>%
      collapse_rows(columns = 1, latex_hline = "none")

    # ridiculous hack to get latex \ back in my phantoms...
    output <-
      output %>%
      str_replace_all(fixed("phantom{X}"), "\\phantom{X}")
  }

  return(output)

}






