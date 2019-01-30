library(tidyverse)
library(testthat)
require(stats)

#===========
# read in
#===========
root <- "C:/Users/Yixin Sun/Documents/Github/regtable"
source(file.path(root, "R/regtable.R"))
source(file.path(root, "R/latex_reader.R"))

#===========
# tests
#===========
count_decimals <- function(no){
	nchar(gsub("(.*\\.)|([0]*$)", "", as.character(no))) 
}

# want a function that takes in a parsnip object, extracts coefficients
# and dimensions, and compares it to the latex output
regtable_test <- function(df, r, f, ...){
	test_fit <- fit_with(df, r, f, ...)[[1]] 

	coef_names <- 
	  names(coef(test_fit)) 

	latex_output <- 
	  list(test_fit) %>%
	  regtable(., est = coef_names, output_format = "latex") %>%
	  latex_reader() %>%
	  pluck("coef") %>%
	  mutate_at(vars(-term, -type), funs(str_replace_all(., "\\(|\\)", ""))) %>%
	  mutate_at(vars(-term, -type), as.numeric) %>%
	  spread(type, V2) %>%
	  select(term, estimate, std.error)

	dec <- unique(count_decimals(latex_output$estimate))

	coef <- 
	  tidy(test_fit) %>%
	  select(term, estimate, std.error) %>%
	  mutate_if(is.numeric, funs(round(., digits = dec)))

	eval(bquote(expect_equal(coef, latex_output)))
}


# use freeny - Freeny's Revenue Data
test_that("testing single model, single independent variable", {
	regtable_test(freeny, felm, list(formula(y ~ lag.quarterly.revenue)))
	regtable_test(freeny, lm, list(formula(y ~ lag.quarterly.revenue)))
})