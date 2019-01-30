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
# use freeny - Freeny's Revenue Data

# want a function that takes in a parsnip object, extracts coefficients
# and dimensions, and compares it to the latex output
regtable_test <- function(df, r, f, ...){
	test_fit <- fit_with(df, r, f, ...)[[1]] 

	coef <- tidy(test_fit)
	coef_names <- names(coef(test_fit))

	latex_output <- 
	  list(test_fit) %>%
	  regtable(., est = coef_names, output_format = "latex") %>%
	  latex_reader() %>%
	  pluck("coef") %>%
	  mutate_at(vars(-term, -type), funs(str_replace_all(., "\\(|\\)", "")))
	  spread(type, V2)

	return(list(model_coef = coef, latex_coef = latex_output))
}

test_that("testing single model, single independent variable", {
	model <- regtable_test(freeny, felm, list(formula(y ~ lag.quarterly.revenue)))

	test_that("one", {
		expect_lte(8, 9)
	})

	test_that("two", {
		expect_gte(8, 9)
	})
})