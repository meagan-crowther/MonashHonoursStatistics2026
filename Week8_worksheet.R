####### WORKSHEET - Mediation #######

library(data.table)
library(JWileymisc)
library(lavaan)


# We are going to create our own data here
set.seed(123) ## Do not change this or you will get a different data set

N <- 200
X <- rnorm(N)
M <- 0.7 * X + rnorm(N, sd = 0.3)
Y <- 0.8 * M + rnorm(N, sd = 0.3)

d <- data.table(
  gpa = X,
  selfesteem = M,
  happiness = Y
)


#We will specify our model
mod_med <- "
  # a path: X -> M
  selfesteem ~ a*gpa

  # b path and direct effect: M -> Y and X -> Y
  happiness ~ b*selfesteem + cp*gpa

  # indirect, direct, total effects, and proportion mediated
  indirect := a*b
  direct   := cp
  total    := direct + indirect
  prop     := indirect / total
"

fit_med <- lavaan::sem(mod_med, data = d, meanstructure = TRUE)

summary(fit_med, standardized = TRUE, rsquare = TRUE)

## From this summary remember to look at the a pathway, b pathway and cprime
## We also want look at the direct and indirect effects.

## Bootstrapping confidence intervals
set.seed(1234)

fit_med_boot <- lavaan::sem(
  mod_med,
  data = d,
  meanstructure = TRUE,
  se = "bootstrap",
  bootstrap = 100
)

# Percentile bootstrap CIs
pe_boot <- parameterEstimates(
  fit_med_boot,
  ci = TRUE,
  level = 0.95,
  boot.ci.type = "perc"
)

# Show the bootstrap CIs for the defined effects
pe_boot[pe_boot$lhs %in% c("indirect", "direct", "total", "prop_med"),
        c("lhs", "est", "se", "ci.lower", "ci.upper")]

# Which (if any) of these bootstrapped confidence intervals indicate significant relationships? (Hint. do any of them not cross zero?)
# A:


## Let's look at all the output together and have a go a interpreting our model:

# To examine whether ___ statistically mediates the association between ___ and ___, a simple mediation model was estimated using the ____ package.
# The predictor, ____, was/was not significantly associated with the mediator, (path a: β = ____, p = ____). 
# The mediator was/ was not significantly associated with the outcome,  ____ ____ (path b: β = ____, p = ____).
# The indirect effect (a x b) was β = ____ with a bootstrapped 95% CI of [___,___], indicating that the mediation effect was/was not statistically significant.
# The direct effect of ___ on ___ remained statistically significant/not significant (β = ____, p = ____), suggesting that the mediation was partial/full/absent.
# Overall, these results indicate that individuals higher in ___ tend to report ___ levels of ___, in part because they also report ____ levels of ___.
