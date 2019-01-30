library(tidyverse)
library(testthat)
require(stats)
library(modelr)
library(lfe)
library(broom)
library(kableExtra)
library(lmtest)

#===========
# read in
#===========

root <- getwd()
while(basename(root) != "regtable") {
  root <- dirname(root)
}

source(file.path(root, "R", "regtable.R")) 
source(file.path(root, "R", "read_latex.R")) 

#===========
# functions
#===========

test_model <- function(model_list, test_statement, est, est_names = NULL, 
	extra_rows = NULL) {

	test_that(test_statement, {

		latex_output <-
				model_list %>%
				regtable(., est = est,  
					est_names = est_names, 
					output_format = "latex", 
					extra_rows = extra_rows)

		latex_file <- file.path(root, 'R', 'tests', 'latex', 
			'temp_regression.tex')

		writeLines(latex_output, latex_file)

		test_that("testing coefficient equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1
				print(count)
				latex_coef <- read_latex(latex_file, output = 'coef') %>% 
					filter(type=='coef') %>% 
					select(-c(est_name, type)) %>% 
					pull(count) %>% 
					as.double()
				model_coef <-  model_list[[count]] %>% 
					summary() %>% 
					coef() %>% 
					.[-1,1] %>% 
					as.double() %>% 
					round(3)
				if (length(model_coef) < length(latex_coef)) {
					model_coef <-  model_list[[count]] %>% 
						summary() %>% 
						coef() %>% 
						.[,1] %>% 
						as.double() %>% 
						round(3)
				}
				print(latex_coef)
				print(model_coef) 
				expect_equal(model_coef, latex_coef)
			}
		})

		test_that("testing standard error equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1
				latex_se <- read_latex(latex_file, output = 'coef') %>% 
					filter(type=='se') %>% 
					select(-c(est_name, type)) %>% 
					pull(count) %>% 
					str_replace_all("\\)", '') %>%
					str_replace_all("\\(", '') %>%
					as.double()
				model_se <-  model_list[[count]] %>% 
					summary() %>% 
					coef() %>% 
					.[-1,2] %>% 
					as.double() %>% 
					round(3)
				if (length(model_se) < length(latex_se)) {
					model_se <-  model_list[[count]] %>% 
						summary() %>% 
						coef() %>% 
						.[,2] %>% 
						as.double() %>% 
						round(3)
				}
				expect_equal(model_se, latex_se)
			}
		})

		test_that("testing projected R^2 equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1
				latex_projected_R2 <- read_latex(latex_file, 
					output = 'stats') %>% 
					filter(stats_name=='Proj. $R^2$') %>% 
					select(-stats_name) %>% 
					pull(count) %>% 
					as.double()
				model_projected_R2 <- model_list[[count]] %>% 
					summary() %>% 
					.$adj.r.squared %>% 
					round(3)
				expect_equal(model_projected_R2, latex_projected_R2)
			}
		})

		test_that("testing N equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1
				latex_N <- read_latex(latex_file, output = 'stats') %>% 
					filter(stats_name=='N') %>% 
					select(-stats_name) %>% 
					pull(count) %>% 
					as.double()
				if (class(model_list[[count]])=='felm') {
					model_N <- model_list[[count]]$N
				} else {
					model_N <- model_list[[count]] %>% nobs()
				}
				expect_equal(model_N, latex_N)
			}
		})
	
	})
}

#===========
# examples
#===========

lm_fits <- mtcars %>% fit_with(lm, formulas(~disp,
		one = ~drat, 
		two = ~drat + cyl,
		three = ~drat * cyl,
		four = add_predictors(three, ~am), 
		five = add_predictors(three, ~am, ~vs)
		))

felm_fits <- mtcars %>% fit_with(felm, formulas(~disp,
		one = ~drat, 
		two = ~drat + cyl,
		three = ~drat * cyl,
		four = add_predictors(three, ~am), 
		five = add_predictors(three, ~am, ~vs)
		))

model1 <- mtcars %>% 
	felm(disp ~ drat + cyl , data = .)

model2 <- mtcars %>% 
	rownames_to_column('company') %>% 
	mutate(company = word(company)) %>% 
	felm(disp ~ drat + cyl | company, data = .)

model3 <- mtcars %>% 
	rownames_to_column('company') %>% 
	mutate(company = word(company)) %>% 
	felm(disp ~ drat + cyl | company + gear, data = .)

#===========
# evaluate tests
#===========

test_model(list(lm_fits$one), 
	"testing single lm model, one independent variable", 
	est = 'drat')

test_model(list(lm_fits$two), 
	"testing single lm model, two independent variables", 
	est = c('drat', 'cyl'))

test_model(list(lm_fits$three), 
	"testing single lm model, three independent variables", 
	est = c('drat', 'cyl', 'drat:cyl'))

test_model(list(lm_fits$four), 
	"testing single lm model, four independent variables", 
	est = c('drat', 'cyl', 'am', 'drat:cyl'))

test_model(list(lm_fits$five), 
	"testing single lm model, five independent variables", 
	est = c('drat', 'cyl', 'am', 'vs', 'drat:cyl'))

test_model(list(felm_fits$one), 
	"testing single felm model, one independent variable", 
	est = 'drat')

test_model(list(felm_fits$two), 
	"testing single felm model, two independent variables", 
	est = c('drat', 'cyl'))

test_model(list(felm_fits$three), 
	"testing single felm model, three independent variables", 
	est = c('drat', 'cyl', 'drat:cyl'))

test_model(list(felm_fits$four), 
	"testing single felm model, four independent variables", 
	est = c('drat', 'cyl', 'am', 'drat:cyl'))

test_model(list(felm_fits$five), 
	"testing single felm model, five independent variables", 
	est = c('drat', 'cyl', 'am', 'vs', 'drat:cyl'))

test_model(list(model1, model2), 
	"testing dual felm model, two independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company"))) 

test_model(list(model1, model2, model3), 
	"testing tripl felm model, two independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company", "Company + Gear"))) 

