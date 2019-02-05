#' @title Stack Multiple Regressions Outputs
#'
#' @description This routine takes in multiple outputs from regtable and 
#' arranges them into one output. The function stacks the different coefficients 
#' and summary statistics, while the extra_rows are grouped at the bottom of the 
#' regtable_stack() output. 
#'
#' @param final_tables list of output produced by regtable, which includes 
#'    \itemize{
#'      \item \emph{output}: a dataframe version of the regtable results
#'      \item \emph{model_names}: either the character vector specified by 
#'        \emph{mnames} in regtable or a function generated vector of numeric 
#'        IDs for the models
#'    } 
#'    To output a dataframe in regtable, pass in "df" for the output_format.
#' 
#' @param table_names A character vector specifying the names of the objects in
#'    final_tables. These names are concatenated with the coefficent names in 
#'    the output. 
#' @param output_format A string passed to kable() that specifies the format of
#'    the table output. The options are latex, html, markdown, pandoc, and rst.
#'    The default is latex.
#' @param note A character string if a footnote is to be added to the end of the
#'    table.
#' @param header A character vector to be passed into 
#'    \code{\link[kableExtra:add_header_above]{add_header_above}} that creates
#'    a new header row. This should have length equal to ms from regtable()
#' 
#' @examples
#' # create covariates
#' x1 <- rnorm(1000)
#' x2 <- rnorm(length(x1))
#' 
#' ## fixed effects
#' fe <- factor(sample(20, length(x1), replace=TRUE))
#' 
#' ## effects for fe
#' fe_effs <- rnorm(nlevels(fe))
#' 
#' ## creating left hand sides y1 and y2
#' u <- rnorm(length(x1))
#' y1 <- 2 * x1 + x2 + fe_effs[fe] + u
#' y2 <- 3 * x1 + x2 + fe_effs[fe] + u
#' 
#' m1 <- felm(y1 ~ x1 + x2 | fe)
#' m2 <- lm(y1 ~ x1 + x2)
#' 
#' n1 <- felm(y2 ~ x1 + x2 | fe)
#' n2 <- lm(y2 ~ x1 + x2)
#' 
#' ## generate output from regtable
#' r1 <- regtable(list(m1, m2), est = "x1", 
#'        output_format = "df")
#' r2 <- regtable(list(n1, n2), est = "x1", 
#'        output_format = "df")
#' 
#' regtable_stack(list(r1, r2), table_names = c("1", "2"), output_format = "rst")
#' 
#' 
#' @importFrom magrittr %>%
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_df map2_df reduce
#' @importFrom knitr kable
#' @importFrom kableExtra collapse_rows row_spec add_footnote
#' @importFrom stringr str_replace_all
#' @import dplyr
#' @importFrom broom glance
#' @name regtable_stack
#' 

#' @export
#' @rdname regtable_stack

regtable_stack <- function(final_tables, table_names = NULL, output_format = "latex", 
  note = NULL, header = NULL){

  if(!is.null(table_names)){
    final_df <-
      map2_df(final_tables, table_names, 
        function(x, y) mutate(x$output, table_name = y))
  } else{
    final_df <- 
      map_df(final_tables, function(x) c(x$output)) %>%
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

  output <-
    output %>%
    kable(format = output_format,
          booktabs = TRUE,
          col.names = colnames(.),
          linesep = "",
          escape = FALSE,
          align = c('l', rep('c', length(final_tables$model_names)))) %>%
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

    output <-
      output %>%
      str_replace_all(fixed("phantom{X}"), "\\phantom{X}")
  }

  return(output)

}