## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(ggplot2)
library(data.table)
library(haldensify)
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
density_hal <- haldensify(
  A = example_data$A,
  W = example_data$W,
  n_bins = c(5, 15),
  grid_type = "equal_range",
  lambda_seq = exp(seq(-1, -10, length = 100))
)

## ----plot_risk_haldensify, message=FALSE, warning=FALSE-----------------------
emp_risk_dat <- as.data.table(list(
  lambda = density_hal$cv_hal_fits_tune_opt$lambda_seq,
  risk = density_hal$cv_hal_fits_tune_opt$emp_risks
))
p_risk <- ggplot(emp_risk_dat, aes(x = lambda, y = risk)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Lambda (L1 regularization parameter)") +
  ylab("Empirical Risk") +
  ggtitle("Empirical risk of HAL conditional density estimators")
p_risk

## ----plot_haldensify, message=FALSE, warning=FALSE----------------------------
# predictions to recover conditional density of A|W
new_a <- seq(-4, 4, by = 0.01)
new_dat <- as.data.table(list(a = new_a,
                              w_neg = rep(-2, length(new_a)),
                              w_zero = rep(0, length(new_a)),
                              w_pos = rep(2, length(new_a))))
new_dat[, pred_w_neg := predict(density_hal, new_A = new_dat$a,
                                new_W = new_dat$w_neg)]
new_dat[, pred_w_zero := predict(density_hal, new_A = new_dat$a,
                                 new_W = new_dat$w_zero)]
new_dat[, pred_w_pos := predict(density_hal, new_A = new_dat$a,
                                new_W = new_dat$w_pos)]

# visualize results
dens_dat <-  melt(new_dat, id = c("a"),
                  measure.vars = c("pred_w_pos", "pred_w_zero", "pred_w_neg"))
p_dens <- ggplot(dens_dat, aes(x = a, y = value, colour = variable)) +
  geom_point() +
  geom_line() +
  stat_function(fun = dnorm, args = list(mean = -2, sd = 0.5),
                colour = "blue", linetype = "dashed") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 0.5),
                colour = "darkgreen", linetype = "dashed") +
  stat_function(fun = dnorm, args = list(mean = 2, sd = 0.5),
                colour = "red", linetype = "dashed") +
  xlab("Observed value") +
  ylab("Predicted probability") +
  ggtitle("Conditional density p(A|W)") +
  theme_bw() +
  theme(legend.position = "none")
p_dens

