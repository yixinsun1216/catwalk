library(lfe)

## create covariates
x1 <- rnorm(1000)
x2 <- rnorm(length(x1))

## fixed effects
fe <- factor(sample(20, length(x1), replace=TRUE))

## effects for fe
fe_effs <- rnorm(nlevels(fe))

## creating left hand side y
u <- rnorm(length(x1))
y <- 2 * x1 + x2 + fe_effs[fe] + u

m <- felm(y ~ x1 + x2 | fe)

regtable(m, "x1", mnames = "(1)", stats = c("N", "r2"),
         stats_names = c("N", "$R^2$"), sig_stars = TRUE)
