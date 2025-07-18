% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core-functions.R
\name{boundary_test}
\alias{boundary_test}
\title{Boundary test for standard deviation}
\usage{
boundary_test(
  sd,
  n_obs,
  mean,
  min_val,
  max_val,
  m_prec = NULL,
  sd_prec = NULL,
  n_items = 1,
  return_range = FALSE
)
}
\arguments{
\item{sd}{The standard deviation of the distribution}

\item{n_obs}{The number of observations (sample size)}

\item{mean}{The mean of the distribution}

\item{min_val}{The minimum value}

\item{max_val}{The maximum value}

\item{m_prec}{The precision of the mean, as number of digits after the decimal point.
If not provided, taken based on the significant digits of \code{mean} - so only needed if reported mean ends in 0}

\item{sd_prec}{The precision of the standard deviation, again only needed if
reported standard deviation ends in 0.}

\item{n_items}{Number of items in scale, if distribution represents scale averages.
Defaults to 1, which represents any single-item measure.}

\item{return_range}{(Optional) If \code{TRUE}, the function returns a numeric vector with the SD possible range.}
}
\value{
Logical \code{TRUE} if the standard deviation is within the possible
range, and \code{FALSE} otherwise, unless \code{return_range}, in which case a
numeric vector with the lower and upper bounds of the possible SD range is returned.
}
\description{
This function tests whether a given standard deviation is within the range that is
theoretically possible for a given sample size and mean, assuming responses are bounded
by a minimum and maximum scale value. It provides a simple TRUE/FALSE
check. Note that SDs within this range may still be impossible due to their granularity,
so for samples where the sample size (x number of items) is below 10 to the power of the
precision of the reported SD, this should only be a precursor to \code{GRIMMER_test()}
}
\examples{

# For a 7-point scale with N = 20 and Mean = 4, an SD of 1.5 is within the possible range
boundary_test(sd = 1.5, n_obs = 20, mean = 4, min_val = 1, max_val = 7)
# but an SD of 3.5 is not possible
boundary_test(sd = 3.5, n_obs = 20, mean = 4, min_val = 1, max_val = 7)
}
