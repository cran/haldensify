## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(haldensify)
library(data.table)
library(ggplot2)
set.seed(75681)

## ----make_example_data, message=FALSE, warning=FALSE--------------------------
make_example_data <- function(n_obs) {
  W <- runif(n_obs, -4, 4)
  A <- rnorm(n_obs, mean = W, sd = 0.25)
  dat <- as.data.table(list(A = A, W = W))
  return(dat)
}

## ----example_data, message=FALSE, warning=FALSE-------------------------------
# number of observations in our simulated dataset
n_obs <- 200
example_data <- make_example_data(n_obs)

# quick look at the data
head(example_data)

## ----fit_haldensify, message=FALSE, warning=FALSE-----------------------------
haldensify_fit <- haldensify(
  A = example_data$A,
  W = example_data$W,
  n_bins = c(20, 30),
  grid_type = "equal_range",
  lambda_seq = exp(seq(-0.1, -10, length = 250)),
  # the following are passed to hal9001::fit_hal() internally
  max_degree = 4,
  reduce_basis = 1 / sqrt(n_obs)
)
haldensify_fit

## ----plot_risk_haldensify, message=FALSE, warning=FALSE-----------------------
p_risk <- plot(haldensify_fit)
p_risk

## ----plot_haldensify, message=FALSE, warning=FALSE----------------------------
# predictions to recover conditional density of A|W
new_a <- seq(-4, 4, by = 0.05)
new_dat <- as.data.table(list(
  a = new_a,
  w_neg = rep(-2, length(new_a)),
  w_zero = rep(0, length(new_a)),
  w_pos = rep(2, length(new_a))
))
new_dat[, pred_w_neg := predict(haldensify_fit, new_A = new_dat$a,
                                new_W = new_dat$w_neg)]
new_dat[, pred_w_zero := predict(haldensify_fit, new_A = new_dat$a,
                                 new_W = new_dat$w_zero)]
new_dat[, pred_w_pos := predict(haldensify_fit, new_A = new_dat$a,
                                new_W = new_dat$w_pos)]

# visualize results
dens_dat <-  melt(new_dat, id = c("a"),
                  measure.vars = c("pred_w_pos", "pred_w_zero", "pred_w_neg"))

p_dens <- ggplot(dens_dat, aes(x = a, y = value, colour = variable)) +
  geom_point() +
  geom_line() +
  stat_function(fun = dnorm, args = list(mean = -2, sd = 0.25),
                colour = "blue", linetype = "dashed") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 0.25),
                colour = "darkgreen", linetype = "dashed") +
  stat_function(fun = dnorm, args = list(mean = 2, sd = 0.25),
                colour = "red", linetype = "dashed") +
  labs(
    x = "Observed value of W",
    y = "Estimated conditional density",
    title = "Conditional density estimates q(A|W)"
  ) +
  theme_bw() +
  theme(legend.position = "none")
p_dens

