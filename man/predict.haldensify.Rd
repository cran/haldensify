% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.haldensify}
\alias{predict.haldensify}
\title{Prediction method for HAL-based conditional density estimation}
\usage{
\method{predict}{haldensify}(object, ..., new_A, new_W, cv_select = TRUE)
}
\arguments{
\item{object}{An object of class \code{\link{haldensify}}, containing the
results of fitting the highly adaptive lasso for conditional density
estimation, as produced by a call to \code{\link{haldensify}}.}

\item{...}{Additional arguments passed to \code{predict} as necessary.}

\item{new_A}{The \code{numeric} vector or similar of the observed values for
which a conditional density estimate is to be generated.}

\item{new_W}{A \code{data.frame}, \code{matrix}, or similar giving the
values of baseline covariates (potential confounders) for the conditioning
set of the observed values \code{A}.}

\item{cv_select}{A \code{logical} indicating whether to return the predicted
density for the value of the regularization parameter selected by global
cross-validation. The default is \code{TRUE}. When set to \code{FALSE}, a
matrix of predicted densities is returned, with each column corresponding
to a value of the regularization parameter less than or equal to the choice
made by the global cross-validation selector.}
}
\value{
A \code{numeric} vector of predicted conditional density values from
 a fitted \code{haldensify} object.
}
\description{
Prediction method for HAL-based conditional density estimation
}
\details{
Method for computing and extracting predictions of the conditional
 density estimates based on the highly adaptive lasso estimator, returned as
 an S3 object of class \code{haldensify} from \code{\link{haldensify}}.
}
\examples{
# simulate data: W ~ U[-4, 4] and A|W ~ N(mu = W, sd = 0.5)
n_train <- 50
w <- runif(n_train, -4, 4)
a <- rnorm(n_train, w, 0.5)
# HAL-based density estimator of A|W
mod_haldensify <- haldensify(
  A = a, W = w, n_bins = 3,
  lambda_seq = exp(seq(-1, -10, length = 50))
)
# predictions to recover conditional density of A|W
new_a <- seq(-4, 4, by = 0.1)
new_w <- rep(0, length(new_a))
pred_dens <- predict(mod_haldensify, new_A = new_a, new_W = new_w)
}
