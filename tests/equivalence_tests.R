library(tidyverse)
library(testthat)
require(stats)
library(lfe)
library(kableExtra)
library(lmtest)
library(modelr)
library(broom)

#===========
# read in
#===========

root <- getwd()
while (basename(root) != 'catwalk'){
	root <- dirname(root)
}

source(file.path(root, "R", "regtable.R")) 
source(file.path(root, "R", "read_latex.R")) 
source(file.path(root, "R", "random_forest_utils.R")) 

#===========
# functions
#===========
count_decimals <- function(no){
	decimals <- nchar(gsub("(.*\\.)|(*$)", "", as.character(no)))
	return (decimals)
}

custom_expect_equal <- function(model_object, latex_object, 
	count, est = NULL) {
	model <- quasi_label(enquo(model_object))
	model$n <- length(model$val)
	model$name <- 'model object'

	latex <- quasi_label(enquo(latex_object))
	latex$n <- length(latex$val)
	latex$name <- 'latex object'

	expect(model$n == latex$n,
	    sprintf("%s has %i numbers, but %s has %i numbers.", model$name, 
	    	model$n, latex$name, latex$n)
	    )

	order <- c('1st', '2nd', '3rd', '4th', '5th', 
		'6th', '7th', '8th', '9th', '10th')

	for (i in 1:latex$n) {
		expect(model$val[i] == latex$val[i],
			ifelse(is.null(est), 
		    sprintf("The %s %s has a value of %f, 
		    	but the %s %s has a value of %i.
		    	Difference: %f", order[count], model$name, 
		    	model$val[i], order[count], latex$name, 
		    	latex$val[i], model$val[i]-latex$val[i]), 
		    sprintf("The %s %s has a value of %f for variable %s, 
		    	but the %s %s has a value of %i for variable %s.
		    	Difference: %f", order[count], model$name, 
		    	model$val[i], est[i], order[count], latex$name, 
		    	latex$val[i], est[i], model$val[i]-latex$val[i]))
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

		test_that("testing coefficient equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1

				latex_coef <- 
					read_latex(latex_output, output = 'coef') %>% 
					filter(type=='coef') %>% 
					select(-c(est_name, type)) %>% 
					pull(count) %>% 
					as.double()

				decimals <- max(count_decimals(latex_coef))
				model_coef <-  
					model_list[[count]] %>% 
					summary() %>% 
					coef() %>% 
					.[,1] 
				if (length(model_coef)>1) {
					model_coef <- 
						model_coef %>% 
						.[est] 
				}
				model_coef <- 
					model_coef %>% 
					as.double() %>% 
					round(decimals)
				custom_expect_equal(model_coef, latex_coef, count, est = est)
			}
		})

		test_that("testing standard error equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1

				latex_se <- 
					read_latex(latex_output, output = 'coef') %>% 
					filter(type=='se') %>% 
					select(-c(est_name, type)) %>% 
					pull(count) %>% 
					str_replace_all("\\)", '') %>%
					str_replace_all("\\(", '') %>%
					as.double()
        
				decimals <- max(count_decimals(latex_se))

				model_se <-  
					model_list[[count]] %>% 
					summary() %>% 
					coef() %>% 
					.[,2]

				if (length(model_se)>1) {
					model_se <- 
						model_se %>% 
						.[est] 
				}
				model_se <- 
					model_se %>% 
					as.double() %>% 
					round(decimals)

				custom_expect_equal(model_se, latex_se, count, est = est)
			}
		})

		test_that("testing projected R^2 equivalence", {
			adj_name <- regex("^adj", ignore_case = TRUE)

			latex_projected_R2 <- 
			  read_latex(latex_output, output = 'stats') %>%
			  filter(stats_name=='Proj. $R^2$') %>%
			  gather(key, stats, -stats_name) %>%
			  select(stats)

			model_projected_R2 <-
			  model_list %>%
			  tibble() %>%
			  rename(model = 1) %>%
			  mutate(stats = map(model, function(x) unclass(summary(x))), 
			  	stats_name = map(model, function(x) names(summary(x)))) %>%
			  unnest(stats, stats_name) %>%
			  filter(str_detect(stats_name, adj_name)) %>%
			  mutate(stats = unlist(stats)) %>%
			  select(stats)

			count = 0
			while (count<length(model_list)) {
				count = count + 1
				latex_projected_R2 <- read_latex(latex_output, 
					output = 'stats') %>% 
					filter(stats_name=='Proj. $R^2$') %>% 
					select(-stats_name) %>% 
					pull(count) %>% 
					as.double()

				decimals <- max(count_decimals(latex_projected_R2))
				model_projected_R2 <- model_list[[count]] %>% 
					summary() %>% 
					.$adj.r.squared %>% 
					round(decimals)

				custom_expect_equal(model_projected_R2, latex_projected_R2, 
					count)
			}
		})

		test_that("testing N equivalence", {
			count = 0
			while (count<length(model_list)) {
				count = count + 1
				latex_N <- read_latex(latex_output, output = 'stats') %>% 
					filter(stats_name=='N') %>% 
					select(-stats_name) %>% 
					pull(count) %>% 
					as.double()
				if (class(model_list[[count]])=='felm') {
					model_N <- model_list[[count]]$N
				} else {
					model_N <- model_list[[count]] %>% nobs()
				}
				custom_expect_equal(model_N, latex_N, count)
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
		five = add_predictors(three, ~am, ~vs)))

felm_fits <- mtcars %>% fit_with(felm, formulas(~disp,
		one = ~drat, 
		two = ~drat + cyl,
		three = ~drat * cyl,
		four = add_predictors(three, ~am), 
		five = add_predictors(three, ~am, ~vs)))

model1 <- mtcars %>% 
	felm(disp ~ drat + cyl , data = .)

model1.1 <- mtcars %>% 
	lm(disp ~ drat + cyl , data = .)

model2 <- mtcars %>% 
	rownames_to_column('company') %>% 
	mutate(company = word(company)) %>% 
	felm(disp ~ drat + cyl | company, data = .)

model3 <- mtcars %>% 
	rownames_to_column('company') %>% 
	mutate(company = word(company)) %>% 
	felm(disp ~ drat + cyl | company + gear, data = .)

rf_model1 <-  mtcars %>% 
	rownames_to_column('company') %>% 
	mutate(company = word(company)) %>% 
	rf_semipar(disp ~ drat | company + gear, .)

rf_model2 <-  mtcars %>% 
	rownames_to_column('company') %>% 
	mutate(company = word(company)) %>% 
	rf_semipar(disp ~ drat + cyl | company + gear, .)

rf_model3 <-  mtcars %>% 
	rownames_to_column('company') %>% 
	mutate(company = word(company)) %>% 
	rf_semipar(disp ~ drat + cyl + drat:cyl| company + gear, .)

#===========
# evaluate tests
#===========

# 1 model, 1-5 ind. variables, lm

test_model(list(lm_fits$one), 
	"testing 1 lm model, 1 independent variable", 
	est = 'drat')

test_model(list(lm_fits$two), 
	"testing 1 lm model, 2 independent variables", 
	est = c('drat', 'cyl'))

test_model(list(lm_fits$three), 
	"testing 1 lm model, 3 independent variables", 
	est = c('drat', 'cyl', 'drat:cyl'))

test_model(list(lm_fits$four), 
	"testing 1 lm model, 4 independent variables", 
	est = c('drat', 'cyl', 'am', 'drat:cyl'))

test_model(list(lm_fits$five), 
	"testing 1 lm model, 5 independent variables", 
	est = c('drat', 'cyl', 'am', 'vs', 'drat:cyl'))

# 1 model, 1-5 ind. variables, felm

test_model(list(felm_fits$one), 
	"testing 1 felm model, 1 independent variable", 
	est = 'drat')

test_model(list(felm_fits$two), 
	"testing 1 felm model, 2 independent variables", 
	est = c('drat', 'cyl'))

test_model(list(felm_fits$three), 
	"testing 1 felm model, 3 independent variables", 
	est = c('drat', 'cyl', 'drat:cyl'))

test_model(list(felm_fits$four), 
	"testing 1 felm model, 4 independent variables", 
	est = c('drat', 'cyl', 'am', 'drat:cyl'))

test_model(list(felm_fits$five), 
	"testing 1 felm model, 5 independent variables", 
	est = c('drat', 'cyl', 'am', 'vs', 'drat:cyl'))

# 2 models, 2 ind. variables, felm

test_model(list(model1, model2), 
	"testing 2 felm models, 2 independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company"))) 

# 3 models, 2 ind. variables, felm

test_model(list(model1, model2, model3), 
	"testing 3 felm models, 2 independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company", "Company + Gear"))) 

# 3 models, 2 ind. variables, lm/felm mix

test_model(list(model1.1, model2, model3), 
	"testing 3 felm models, 2 independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company", "Company + Gear"))) 


# 3 models, 2 ind. variables, lm/felm/rf mix

test_model(list(model1.1, model2, rf_model2), 
	"testing 3 felm models, 2 independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company", "Company + Gear"))) 

# 1 model, 1-3 ind. variables, random forest

test_model(list(rf_model1), 
	"testing 1 rf_semipar model, 1 independent variable", 
	est = c('drat')) 

test_model(list(rf_model2), 
	"testing 1 rf_semipar model, 2 independent variables", 
	est = c('drat', 'cyl')) 

test_model(list(rf_model3), 
	"testing 1 rf_semipar model, 3 independent variables", 
	est = c('drat', 'cyl', 'drat:cyl')) 

# 1 model, 2 ind. variables, 1 ommitted, lm
test_model(list(lm_fits$two), 
	"testing 1 lm model, 2 independent variables, only 1 kept", 
	est = c('cyl'))

# 2 models, 2-3 ind. variables, 1-2 ommitted, lm
test_model(list(lm_fits$two, lm_fits$three), 
	"testing 2 lm models, 2-3 independent variables, only 1 kept", 
	est = c('cyl'), 
	extra_rows = list("Interaction" = c("N", "Y")))

# 2 models, 2-3 ind. variables, 1-2 ommitted, lm/rf mix
test_model(list(lm_fits$two, rf_model2), 
	"testing 2 models, lm/rf mix, 2 independent variables, only 1 kept", 
	est = c('cyl'), 
	extra_rows = list("Method" = c("lm", "rf")))

# 1 model, 1 ind. variables, rename variable, lm
test_model(list(lm_fits$one), 
	"testing 1 lm model, 1 independent variable", 
	est = 'drat', 
	est_names = c('Drat Rename'))

