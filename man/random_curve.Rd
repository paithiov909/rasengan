% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curve-random.R
\name{random_curve}
\alias{random_curve}
\title{Generate a random curve}
\usage{
random_curve(n, power = 8, hurst_index = 0.5, smoothing = 0.4, ...)
}
\arguments{
\item{n}{Number of points.}

\item{power}{Integer.}

\item{hurst_index}{Numeric in range \verb{(0, 1)}.}

\item{smoothing}{Smoothing parameter to be passed to \code{\link[stats:loess]{stats::loess()}} as \code{span}.}

\item{...}{Additional arguments to be passed to \code{\link[stats:loess]{stats::loess()}}.}
}
\value{
A data frame with columns \code{x}, \code{y}, and \code{t}.
}
\description{
Generates a random smooth curve using fractional Brownian bridges.
This function fits a series of points that are generated by \code{\link[=fbbridge_from]{fbbridge_from()}}
using \code{\link[stats:loess]{stats::loess()}} and returns predicted points for \code{n} time steps.
}
