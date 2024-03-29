% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/truncated_severity.R
\name{autoplot.truncated_dist}
\alias{autoplot.truncated_dist}
\title{Automatically create a ggplot for objects obtained from fit_truncated_dist()}
\usage{
\method{autoplot}{truncated_dist}(
  object,
  geom_ecdf = c("point", "step"),
  xlab = NULL,
  ylab = NULL,
  ylim = c(0, 1),
  xlim = NULL,
  print_title = TRUE,
  print_dig = 2,
  print_trunc = 2,
  ...
)
}
\arguments{
\item{object}{object univariate object produced by \code{fit_truncated_dist()}}

\item{geom_ecdf}{the geometric object to use display the data (point or step)}

\item{xlab}{the title of the x axis}

\item{ylab}{the title of the y axis}

\item{ylim}{two numeric values, specifying the lower limit and the upper
limit of the scale}

\item{xlim}{two numeric values, specifying the left limit and the right
limit of the scale}

\item{print_title}{show title (default to TRUE)}

\item{print_dig}{number of digits for parameters in title (default 2)}

\item{print_trunc}{number of digits for truncation values to print}

\item{...}{other plotting parameters to affect the plot}
}
\value{
a ggplot2 object
}
\description{
Takes an object produced by \code{fit_truncated_dist()}, and plots
the available input.
}
\author{
Martin Haringa
}
