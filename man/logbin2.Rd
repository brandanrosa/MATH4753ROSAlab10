% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logbin.R
\name{logbin2}
\alias{logbin2}
\title{logbin2}
\usage{
logbin2(theta)
}
\arguments{
\item{theta}{parameter to be sampled}
}
\value{
a vector
}
\description{
log likelihood function for additive binomials
}
\examples{
\dontrun{logbin2=function(theta){log(dbinom(4,prob=theta,size=10))}}
}
