% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymaxlik.R
\name{mymaxlik}
\alias{mymaxlik}
\title{mymaxlik}
\usage{
mymaxlik(lfun, x, param, ...)
}
\arguments{
\item{lfun}{function to be used}

\item{x}{x values for the function}

\item{param}{parameter values to be sampled}

\item{...}{pass additional options to the function}
}
\value{
a plot and a list
}
\description{
a general function that will use the grid approximation to find the maximum likelihood estimate of a one parameter likelihood
}
\examples{
\dontrun{mymaxlik(lfun=logbin, x=c(3,4,5), param=seq(0,1,length=1000))}
}
