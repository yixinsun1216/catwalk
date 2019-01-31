library(tidyverse)
library(testthat)
require(stats)
library(lfe)
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
count_decimals <- function(no){
	nchar(gsub("(.*\\.)|([0]*$)", "", as.character(no))) 
}

coef_tibble <- function(x){
	output <- 
	  x %>%
	  summary %>%
	  coef 
	output <- 
	  tibble(est_name = rownames(output)) %>% 
	  bind_cols(as_tibble(output))
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

		if(is.null(est_names)) {est_names <- est}

		test_that("testing coefficient and se equivalence", {
			latex_coef <- 
			  read_latex(latex_output, output = 'coef') %>%
			  gather(key, value, -type, -est_name) %>%
			  #unite(temp, type, id) %>%
			  spread(type, value) %>%
			  arrange(key) %>%
			  mutate(id = as.numeric(as.factor(key))) %>%
			  select(-key) %>%
			  mutate(se = str_replace_all(se, "\\(|\\)", "")) %>%
			  mutate_at(vars(coef, se), as.numeric)

			dec <- max(count_decimals(latex_coef$coef))

			model_coef <-
			  model_list %>%
			  tibble() %>%
			  rename(model = 1) %>%
			  mutate(coef = map(model, coef_tibble), 
			  	id = row_number()) %>%
			  unnest(coef, .preserve = id) %>%
			  select(est_name, coef = 3, se = 4, id) %>%
			  filter(est_name %in% est_names) %>%
			  mutate_if(is.double, funs(round(., digits = dec)))

			expect_equal(model_coef, latex_coef)
		
		})

my.summary <- summary(DATA$ids)
data.frame(ids=names(my.summary), nums=my.summary)

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
	"testing 2 felm model, two independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company"))) 

# 3 models, 2 ind. variables, felm

test_model(list(model1, model2, model3), 
	"testing tripl felm model, two independent variables", 
	est = c('drat', 'cyl'), 
	extra_rows = list("FE" = c("None", "Company", "Company + Gear"))) 

