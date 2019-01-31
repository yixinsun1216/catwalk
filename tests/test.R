print('1')
library(tidyverse)
print('2')
library(testthat)
print('3')
require(stats)
print('4')
library(lfe)
print('5')
library(kableExtra)
print('6')
library(lmtest)
print('7')

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

count_decimals <- function(no){
	nchar(gsub("(.*\\.)|(*$)", "", as.character(no)))
}

custom_expect_equal <- function(model_object, latex_object) {
	act_model <- quasi_label(enquo(model_object))
	act_model$n <- length(act_model$val)
	act_model$name <- 'model object'

	act_latex <- quasi_label(enquo(latex_object))
	act_latex$n <- length(act_latex$val)
	act_latex$name <- 'latex object'

	expect(act_model$n == act_latex$n,
	    sprintf("%s has %i numbers, but %s has %i numbers.", act_model$name, 
	    	act_model$n, act_latex$name, act_latex$n)
	    )

	for (i in 1:act_latex$n) {
		expect(act_model$val[i] == act_latex$val[i],
		    sprintf("The %s has a value of %f in position %i, 
		    	but the %s has a value of %i in that position.
		    	Difference: %f", act_model$name, 
		    	act_model$val[i], i, act_latex$name, 
		    	act_latex$val[i], act_model$val[i]-act_latex$val[i])
	    )
	}
}

test_model <- function(model_list, test_statement, est, est_names = NULL, 
	extra_rows = NULL) {

	test_that(test_statement, {

		latex_output <-
				model_list %>%
				regtable(., est = est,  
					est_names = est_names, 
					output_format = "latex", 
					extra_rows = extra_rows)

		latex_file <- file.path(root, 'tests', 'latex', 
			'temp_regression.tex')

		writeLines(latex_output, latex_file)

		test_that("testing coefficient equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1
				latex_coef <- read_latex(latex_file, output = 'coef') %>% 
					filter(type=='coef') %>% 
					select(-c(est_name, type)) %>% 
					pull(count) %>% 
					as.double()
				decimals = max(count_decimals(latex_coef))
				model_coef <-  model_list[[count]] %>% 
					summary() %>% 
					coef() %>% 
					.[-1,1] %>% 
					as.double() %>% 
					round(decimals)
				if (length(model_coef) < length(latex_coef)) {
					model_coef <-  model_list[[count]] %>% 
						summary() %>% 
						coef() %>% 
						.[,1] %>% 
						as.double() %>% 
						round(decimals)
				}
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
				decimals = max(count_decimals(latex_se))
				model_se <-  model_list[[count]] %>% 
					summary() %>% 
					coef() %>% 
					.[-1,2] %>% 
					as.double() %>% 
					round(decimals)
				if (length(model_se) < length(latex_se)) {
					model_se <-  model_list[[count]] %>% 
						summary() %>% 
						coef() %>% 
						.[,2] %>% 
						as.double() %>% 
						round(decimals)
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
				decimals = max(count_decimals(latex_projected_R2))
				model_projected_R2 <- model_list[[count]] %>% 
					summary() %>% 
					.$adj.r.squared %>% 
					round(decimals)
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

# 1 model, 1-5 ind. variables, lm

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

# 1 model, 1-5 ind. variables, felm

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

# 2 models, 2 ind. variables, felm

test_model(list(model1, model2), 
	"testing dual felm model, two independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company"))) 

# 3 models, 2 ind. variables, felm

test_model(list(model1, model2, model3), 
	"testing tripl felm model, two independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company", "Company + Gear"))) 

