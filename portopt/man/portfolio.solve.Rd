% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portfolio_solution.R
\name{portfolio.solve}
\alias{portfolio.solve}
\title{Analytical solution to Mean-Variance Portfolio.}
\usage{
portfolio.solve(returns, model, covars, var)
}
\arguments{
\item{returns}{A matrix of stock prices}

\item{model}{an object of S4 class "model".}

\item{covars}{output from the function `approx.covar()`}

\item{var}{upper bound of variance.}
}
\description{
Analytical solution to Mean-Variance Portfolio.
}
