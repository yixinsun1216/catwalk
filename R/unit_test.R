library(tidyverse)
library(testthat)
require(stats)

#===========
# read in
#===========
root <- "C:/Users/Yixin Sun/Documents/Github/regtable"
source(file.path(root, "regtable.R"))
source(file.path(root, "latex_reader.R"))

#===========
# tests
#===========
test <- lm(y ~ lag.quarterly.revenue + income.level, freeny)

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
	  latex_reader() 

	return(list coef, latex_output)
}

test_that("testing single model, single independent variable", {
	model <- regetable_test(freeny, "felm", formula(y ~ lag.quartrly.revenue))

	latex_output <-
			list(model) %>%
			regtable(., est = coef_names, 
				output_format = "latex")
	writeLines(latex_output, file.path(root, 'tests', 'latex', 
		'single_regression.tex'))

	test_that("one", {
		expect_lte(8, 9)
	})

	test_that("two", {
		expect_gte(8, 9)
	})
})