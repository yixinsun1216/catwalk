#' @title Random Forest Helper Functions
#'
#' @description Estimate a partially linear model using the robinson 1988 methodology using
#' random forests as the nonparametric regression technology.  The formula
#' structure has three parts: y ~ x | z.  y is the outcome, x are the linear
#' variables and z are the non-parametric variables.  Tt *should* work with
#' whatever transformations you throw at it, but note that the resulting lm
#' object it returns may have column names that are slightly different if you do
#' transformations.  For example, the variables in the output of a model
#' drilled ~ Auction + log(acres) | Cent_Lat + Cent_Long + EffDate will be
#' Auction and *log_acres_*.  ALSO: if you put something more complicated in,
#' like, say, a call to bs() from the splines package, you are going to get
#' pretty crazy names, but it should still work.
#'
#' @param formula The regression formula to evaluate
#' @param data The dataframe that contains all the variables used in the formula

#' @examples
#' height <- runif(100, 60, 78)
#' dad_height <- runif(100, 66, 78)
#' mom_height <- runif(100, 60, 72)
#' df <- tibble(height = height, dad_height = dad_height, 
#'    mom_height = mom_height)
#' 
#' model <- rf_semipar(height ~ dad_height | mom_height, data = df)
#'  
#' @import tidyverse
#' @import Formula
#' @import grf
#' @name rf_semipar
NULL


library(tidyverse)
library(Formula)
library(grf)

#===========================================================================
# RANDOM FOREST HELPER FUNCTIONS
#===========================================================================
# estimate treatment effects with a formula instead of manually passing matrices
# y ~ w | x1 + x2 + x3 means estimate a causal forest for the outcome y, where
# treatment is given by w, and we wish to match on x1, x2, and x3
# to get an average treatment effect (or ATT, etc), pass the results of this
# to the average_treatment_effect() routine in the grf package
causal_forest2 <- function(f, d, ...) {
  f <- Formula(f)

  Y <-
    f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    as.matrix

  W <-
    formula(f, rhs = 1, lhs = 0) %>%
    update(~ 0 + .) %>%    
    model.matrix(d)

  X <-
    formula(f, rhs = 2, lhs = 0) %>%
    update(~ 0 + .) %>%
    model.matrix(d)

  cf <- causal_forest(X, Y, W, ...)

  cf[["formula"]] <- f
  class(cf) <- c("causal_forest", "grf")

  return(cf)
}

# estimate a regression forest with a formula instead of manually passing
# matrices.  the formula y ~ w | x1 + x2 + x3 means estimate a regression forest
# for the outcome y, where the splitting variables are x1, x2, and x3
# to get predicted values, pass the results of this to predict()
regression_forest2 <- function(f, d, ...) {
  f <- Formula(f)

  Y <-
    f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    model.response %>%
    matrix
  
  X <-
    formula(f, rhs = 1, lhs = 0) %>%
    update(~ 0 + .) %>%
    model.matrix(d)
  
  ff <- regression_forest(X, Y, ...)
  
  ff[["formula"]] <- f
  class(rf) <- c("regression_forest", "grf")
  
  return(ff)
}

# method to comute E[Yi|X] for each i in YY.
# returns a list of regression_forest objects.  probably not useful by itself
regression_forest_resid <- function(X, YY, ...) {
  preds <- apply(YY,
                 2,
                 function(y) predict(regression_forest(X, matrix(y), ...))[[1]])
  return(YY - preds)
}

# estimate a residualized random forest.  use this when you want to compute
# Y - E[Y|X] using random forest regression technology.  the formula for this
# can have multiple parts on the LHS.  For example, y1 | y2 ~ x1 + x2 + x3
# will return a tibble where the columns are y1 - E[y1|x1, x2, x3] and
# y2 - E[y2|x1, x2, x3].  like the routine above, I don't *think* this is
# useful on its own, but it is a key component of the semi-parametric code that
# comes later
regression_forest_resid2 <- function(f, d, ...) {
  f <- Formula(f)

  nlhs <- length(all.vars(formula(f, rhs = 0)))
  
  if(nlhs > 1) {
    # do multiple LHS vars
    # approach: convert the LHS expression into a collapsed rhs expression
    # convert LHS' and RHS into matrices
    # pass to regression_forest_resid
    # add names to residualized columns and convert to tibble
    YY <-
      formula(f, rhs = 0, collapse = TRUE) %>%
      as.character %>%
      pluck(2) %>%
      paste("~ 0 +", ., sep = " ") %>%
      as.formula %>%
      model.matrix(d)

    X <-
      formula(f, rhs = 1, lhs = 0) %>%
      update(~ 0 + .) %>%
      model.matrix(d)

    resids <- regression_forest_resid(X, YY, ...)
    colnames(resids) <- colnames(YY)
    return(as_tibble(resids))
  }
  else {
    Y <-
      f %>%
      formula(lhs = 1, rhs = 0) %>%
      as.character %>%
      pluck(2) %>%
      paste("~ 0 +", ., sep = " ") %>%
      as.formula %>%
      model.matrix(d)

    X <-
      formula(f, rhs = 1, lhs = 0) %>%
      update(~ 0 + .) %>%
      model.matrix(d)

    rf <- regression_forest(X, Y, ...)
    resids <- Y - predict(rf)[[1]]
    colnames(resids) <- all.vars(formula(f, lhs = 1, rhs = 0))
    return(as_tibble(resids))
  }
}

# estimate a partially linear model using the robinson 1988 methodology using
# random forests as the nonparametric regression technology.  the formula
# structure has three parts: y ~ x | z.  y is the outcome, x are the linear
# variables and z are the non-parametric variables.  it *should* work with
# whatever transformations you throw at it, but note that the resulting lm
# object it returns may have column names that are slightly different if you do
# transformations.  for example, the variables in the output of a model
# drilled ~ Auction + log(acres) | Cent_Lat + Cent_Long + EffDate will be
# Auction and *log_acres_*.  ALSO: if you put something more complicated in,
# like, say, a call to bs() from the splines package, you are going to get
# pretty crazy names, but it should still work.

#' @export
#' @rdname rf_semipar 
rf_semipar <- function(formula, data, ...) {
  # step 0: ensure fm has no intercept in the parametric part
  formula <-
    formula %>%
    Formula %>%
    update(. ~ 0 + . | .)

  # step 1: get LHS residuals wrt to the RF variables
  lhs_resid <-
    formula %>%
    formula(lhs = 1, rhs = 2) %>%
    regression_forest_resid2(data, ...) %>%
    rename_all(funs(str_replace_all(., regex("[(, =)]"), "_")))
    
  # step 2: get RHS residuals wrt to the RF variables
  rhs_data <-
    formula %>%
    formula(lhs = 0, rhs = 1) %>%
    update(~ 0 + .) %>%
    model.matrix(data) %>%
    as_tibble %>%
    rename_all(funs(str_replace_all(., regex("[(, =)]"), "_")))

  rf_data <-
    formula %>%
    formula(lhs = 0, rhs = 2) %>%
    update(~ 0 + .) %>%
    model.matrix(data) %>%
    as_tibble %>%
    rename_all(funs(str_replace_all(., regex("[(, =)]"), "_")))

  resid_data <- bind_cols(rhs_data, rf_data)

  rhs_resid <-
    paste(paste0(names(rhs_data), collapse = " | "),
          paste0(names(rf_data), collapse = " + "),
          sep = " ~ ") %>%
    formula %>%
    regression_forest_resid2(resid_data, ...)

  # step 3: run OLS and return an LM object
  resid_data <- bind_cols(lhs_resid, rhs_resid)

  ## # add back columns not specifically containedin resid_data so that ex post
  ## # clustering is possible
  ## dnames <- setdiff(names(data), names(resid_data))
  ## dslim <- data[, dnames]

  ## resid_data <- bind_cols(resid_data, dslim)

  resid_fm <-
    paste(names(lhs_resid),
          paste(names(rhs_resid), collapse = " + "),
          sep = " ~ ") %>%
    as.formula %>%
    Formula %>%
    update(. ~ 0 + . | .)
  
  m <-
    resid_fm %>%
    formula(lhs = 1, rhs = 1) %>%
    lm(resid_data)

  return(m)
}

# compute the sample average of log E[y|x_i,w=1] - log E[y|x_i,w=0]
rf_logcausal <- function(X, Y, W, ...) {
  rf <- regression_forest(cbind(W, X), Y, ...)

  # y is the "honest" prediction, which we use for y1 if w==1, and y0 if w==0
  # otherwise we use the predicted regression function values in y1 and y2
  y <- predict(rf, ...)[[1]]

  y1 <- predict(rf, newdata = cbind(rep(1, nrow(X)), X), ...)[[1]]
  y1[W == 1] <- y[W == 1]
  
  y0 <- predict(rf, newdata = cbind(rep(0, nrow(X)), X), ...)[[1]]
  y0[W == 0] <- y[W == 0]

  delta <- log(y1 / y0)
  
  return(mean(delta))
}

rf_logcausal2 <- function(f, d, ...) {
  f <- Formula(f)

  Y <-
    f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    model.response %>%
    matrix

  W <-
    formula(f, rhs = 1, lhs = 0) %>%
    update(~ 0 + .) %>%    
    model.matrix(d)

  X <-
    formula(f, rhs = 2, lhs = 0) %>%
    update(~ 0 + .) %>%
    model.matrix(d)

  return(rf_logcausal(X, Y, W, ...))
}


rf_logcausal_bca <- function(X, Y, W, B, ...) {
    return(bcajack(cbind(Y, W, X), 
                   B,
                   function(Z, ...) rf_logcausal(Z[, 3:ncol(Z)],
                                                 Z[, 1],
                                                 Z[, 2],
                                                 ...),
                   m = 20))
}

rf_logcausal_bca2 <- function(f, d, B, ...) {
  f <- Formula(f)

  Y <-
    f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    model.response %>%
    matrix

  W <-
    formula(f, rhs = 1, lhs = 0) %>%
    update(~ 0 + .) %>%    
    model.matrix(d)

  X <-
    formula(f, rhs = 2, lhs = 0) %>%
    update(~ 0 + .) %>%
    model.matrix(d)

  return(rf_logcausal_bca(X, Y, W, B, ...))
  
}

rf_logcausal2 <- function(f, d, ...) {
  f <- Formula(f)

  Y <-
    f %>%
    formula(lhs = 1, rhs = 0) %>%
    model.frame(d) %>%
    model.response %>%
    matrix

  W <-
    formula(f, rhs = 1, lhs = 0) %>%
    update(~ 0 + .) %>%    
    model.matrix(d)

  X <-
    formula(f, rhs = 2, lhs = 0) %>%
    update(~ 0 + .) %>%
    model.matrix(d)

  return(rf_logcausal(X, Y, W, ...))
}

