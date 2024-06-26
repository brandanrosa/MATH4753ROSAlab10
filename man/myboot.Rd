% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot.R
\name{myboot}
\alias{myboot}
\title{myboot}
\usage{
myboot(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{number of iterations}

\item{x}{x values for the function}

\item{fun}{function to be used}

\item{alpha}{alpha level}

\item{cx}{font size}

\item{...}{pass additional options to the function}
}
\value{
a plot and an invisible list
}
\description{
a function to make interval estimates for parameters by using re-sampling techniques or what has come to be called “bootstrap”, the point estimate is made from the original sample and the interval estimate is made from the re-samples
}
\examples{
\dontrun{myboot(iter=10000,x=ddt$WEIGHT,fun="mean",alpha=0.05,cx=1.5)}
}
