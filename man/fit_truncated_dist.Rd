% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/truncated_severity.R
\name{fit_truncated_dist}
\alias{fit_truncated_dist}
\title{Fit a distribution to truncated severity (loss) data}
\usage{
fit_truncated_dist(
  y,
  dist = c("gamma", "lognormal"),
  left = NULL,
  right = NULL,
  start = NULL,
  print_initial = TRUE
)
}
\arguments{
\item{y}{vector with observations of losses}

\item{dist}{distribution for severity ("gamma" or "lognormal"). Defaults to
"gamma".}

\item{left}{numeric. Observations below this threshold are not present in the
sample.}

\item{right}{numeric. Observations above this threshold are not present in
the sample. Defaults to Inf.}

\item{start}{list of starting parameters for the algorithm.}

\item{print_initial}{print attempts for initial parameters.}
}
\value{
fitdist returns an object of class "fitdist"
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Estimate the original
distribution from truncated data. Truncated data arise frequently in
insurance studies. It is common that only claims above a certain threshold
are known.
}
\examples{
\dontrun{
# Original observations for severity
set.seed(1)
e <- rgamma(1000, scale = 148099.5, shape = 0.4887023)

# Truncated data (only claims above 30.000 euros)
threshold <- 30000
f <- e[e > threshold]

library(dplyr)
library(ggplot2)
data.frame(value = c(e, f),
variable = rep(c("Original data", "Only claims above 30.000 euros"),
               c(length(e), length(f)))) \%>\%
               filter(value < 5e5) \%>\%
               mutate(value = value / 1000) \%>\%
               ggplot(aes(x = value)) +
               geom_histogram(colour = "white") +
               facet_wrap(~variable, ncol = 1) +
               labs(y = "Number of observations",
                    x = "Severity (x 1000 EUR)")

# scale = 156259.7 and shape = 0.4588. Close to parameters of original
# distribution!
x <- fit_truncated_dist(f, left = threshold, dist = "gamma")

# Print cdf
autoplot(x)

# CDF with modifications
autoplot(x, print_dig = 5, xlab = "loss", ylab = "cdf", ylim = c(.9, 1))

est_scale <- x$estimate[1]
est_shape <- x$estimate[2]

# Generate data from truncated distribution (between 30k en 20 mln)
rg <- rgammat(10, scale = est_scale, shape = est_shape, lower = 3e4,
 upper = 20e6)

# Calculate quantiles
quantile(rg, probs = c(.5, .9, .99, .995))
}

}
\author{
Martin Haringa
}
