% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rating_factors.R
\name{rating_factors1}
\alias{rating_factors1}
\title{Include reference group in regression output}
\usage{
rating_factors1(
  model,
  model_data = NULL,
  exposure = NULL,
  colname = "estimate",
  exponentiate = TRUE
)
}
\arguments{
\item{model}{glm object produced by \code{glm()}}

\item{model_data}{data.frame used to create glm object}

\item{exposure}{column in \code{model_data} with exposure}

\item{colname}{name of column with estimates. Defaults to "estimate".}

\item{exponentiate}{Logical indicating whether or not to exponentiate the the coefficient estimates. Defaults to TRUE.}
}
\description{
This extracts coefficients in terms of the original levels of the coefficients rather than the coded variables.
}