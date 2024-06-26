% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gam_construct_tariff_classes.R
\name{construct_tariff_classes}
\alias{construct_tariff_classes}
\title{Construct insurance tariff classes}
\usage{
construct_tariff_classes(
  object,
  alpha = 0,
  niterations = 10000,
  ntrees = 200,
  seed = 1
)
}
\arguments{
\item{object}{fitgam object produced by \code{fit_gam}}

\item{alpha}{complexity parameter. The complexity parameter (alpha) is used
to control the number of tariff classes. Higher values for \code{alpha}
render less tariff classes. (\code{alpha} = 0 is default).}

\item{niterations}{in case the run does not converge, it terminates after a
specified number of iterations defined by niterations.}

\item{ntrees}{the number of trees in the population.}

\item{seed}{an numeric seed to initialize the random number generator (for
reproducibility).}
}
\value{
A list of class \code{constructtariffclasses} with components
\item{prediction}{data frame with predicted values}
\item{x}{name of continuous risk factor for which tariff classes are
constructed}
\item{model}{either 'frequency', 'severity' or 'burning'}
\item{data}{data frame with predicted values and observed values}
\item{x_obs}{observations for continuous risk factor}
\item{splits}{vector with boundaries of the constructed tariff classes}
\item{tariff_classes}{values in vector \code{x} coded according to which
constructed tariff class they fall}
}
\description{
Constructs insurance tariff classes to \code{fitgam} objects
produced by \code{fit_gam}. The goal is to bin the continuous risk factors
such that categorical risk factors result which capture the effect of the
covariate on the response in an accurate way, while being easy to use in a
generalized linear model (GLM).
}
\details{
Evolutionary trees are used as a technique to bin the \code{fitgam}
object produced by \code{fit_gam} into risk homogeneous categories.
This method is based on the work by Henckaerts et al. (2018). See Grubinger
et al. (2014) for more details on the various parameters that
control aspects of the evtree fit.
}
\examples{
\dontrun{
library(dplyr)
fit_gam(MTPL, nclaims = nclaims,
x = age_policyholder, exposure = exposure) |>
   construct_tariff_classes()
}

}
\references{
Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a
priori and a posteriori risk classification in insurance. Advances in
Statistical Analysis, 96(2):187–224. doi:10.1007/s10182-011-0152-7.

Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). evtree:
Evolutionary learning of globally optimal classification and regression trees
in R. Journal of Statistical Software, 61(1):1–29. doi:10.18637/jss.v061.i01.

Henckaerts, R., Antonio, K., Clijsters, M. and Verbelen, R.
(2018). A data driven binning strategy for the construction of insurance
tariff classes. Scandinavian Actuarial Journal, 2018:8, 681-705.
doi:10.1080/03461238.2018.1429300.

Wood, S.N. (2011). Fast stable restricted maximum likelihood and
marginal likelihood estimation of semiparametric
generalized linear models. Journal of the Royal Statistical Society (B)
73(1):3-36. doi:10.1111/j.1467-9868.2010.00749.x.
}
\author{
Martin Haringa
}
