library(mirt)
library(mirtCAT)

# for reproducibility
set.seed(1678934)

n_items <- 50L

# random parameters
pars <- data.frame(
  a1 = rlnorm(n_items, .2, .3),
  d = rnorm(n_items, sd = 1.5)
)

# save the mirt model
example_2pl_mod <- generate.mirt_object(pars, itemtype = "2PL")

# latent ability to generate the response patterns for
theta <- 1.5

# based on given theta, generate plausible response pattern
response_pattern <- generate_pattern(example_2pl_mod, theta)

# simulate the CAT administration for the given response pattern
cat_results <- mirtCAT(
  mo = example_2pl_mod, local_pattern = response_pattern,
  start_item = "MI", method = "MAP", criteria = "MI"
)

# plot with 95% confidence intervals
plot(cat_results, SE = qnorm(.975))
