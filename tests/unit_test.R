library(tidyverse)
library(testthat)
require(stats)

#===========
# read in
#===========
root <- "C:/Users/Yixin Sun/Documents/Github/regtable"
source(file.path(root, "R/regtable.R"))
source(file.path(root, "tests/latex_reader.R"))

#===========
# tests
#===========
count_decimals <- function(no){
	nchar(gsub("(.*\\.)|([0]*$)", "", as.character(no))) 
}

# want a function that takes in a parsnip object, extracts coefficients
# and dimensions, and compares it to the latex output
regtable_test <- function(df, r, f, ...){
	test_fit <- 
	  map2(r, f, function(x, y) fit_with(data = df, x, list(y))) %>%
	  map(pluck(1))

	coef_names <- names(coef(test_fit[[1]])) 

	latex_output <- 
	  test_fit %>%
	  regtable(., est = coef_names, output_format = "latex") %>%
	  latex_reader() %>%
	  pluck("coef") %>%
	  mutate_at(vars(-term, -type), funs(str_replace_all(., "\\(|\\)", ""))) %>%
	  mutate_at(vars(-term, -type), as.numeric) %>%
	  gather(key, value, -type, -term) %>%
	  spread(type, value) %>%
	  arrange(key) %>%
	  select(-key)

	dec <- unique(count_decimals(latex_output$estimate))

	coef <- 
	  test_fit %>% 
	  tibble %>% 
	  rename(model = 1) %>% 
	  mutate(coef_info = map(model, tidy)) %>% 
	  unnest(coef_info) %>%
	  select(term, estimate, std.error) %>%
	  mutate_if(is.numeric, funs(round(., digits = dec)))

	eval(bquote(expect_equal(coef, latex_output)))
}


# use freeny - Freeny's Revenue Data
test_that("testing single model", {
	regtable_test(freeny, list(felm), list(formula(y ~ lag.quarterly.revenue)))
	regtable_test(freeny, list(felm, lm), list(formula(y ~ lag.quarterly.revenue + price.index)))
})