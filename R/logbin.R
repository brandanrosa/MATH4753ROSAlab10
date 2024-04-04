#' logbin
#'
#'  log likelihood function for binomials
#'
#' @param x x values for the function
#' @param param parameter values to be sampled
#'
#' @return a vector
#' @export
#'
#' @importFrom stats dbinom
#'
#' @examples \dontrun{logbin(x = c(3,4,5), prob = param)}
logbin <- function(x, param){
  log(dbinom(x, prob = param, size = 10))
}

#' logpoiss
#'
#' log likelihood function for Poisson distributions
#'
#' @param x x values for the function
#' @param param parameter values to be sampled
#'
#' @return a vector
#' @export
#'
#' @importFrom stats dpois
#'
#' @examples \dontrun{logpoiss(x = c(3,4,5), lambda = param)}
logpoiss <- function(x, param) {
  log(dpois(x, lambda = param))
}

#' logexp
#'
#' log likelihood function for exponentials
#'
#' @param x x values for the function
#' @param param parameter values to be sampled
#'
#' @return a vector
#' @export
#'
#' @importFrom stats dexp
#'
#' @examples \dontrun{logexp(x = c(3,4,5), rate = param)}
logexp <- function(x, param) {
  log(dexp(x, rate = param))
}

#' logbin2
#'
#' log likelihood function for additive binomials
#'
#' @param theta parameter to be sampled
#'
#' @return a vector
#' @export
#'
#' @importFrom stats dbinom
#'
#' @examples \dontrun{logbin2=function(theta){log(dbinom(4,prob=theta,size=10))}}
logbin2 <- function(theta){
  log(dbinom(4,prob=theta,size=10)) + log(dbinom(12,prob=theta,size=20)) + log(dbinom(7, prob = theta, size = 15))
  }


