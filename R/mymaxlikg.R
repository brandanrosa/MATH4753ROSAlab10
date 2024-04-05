#' mymaxlikg
#'
#' a more general function for a grid approximation to make maximum likelihood estimations, provided you supply the appropriate helper function
#'
#' @param lfun function to be used
#' @param theta parameter to be estimated
#'
#' @return a plot and a list
#' @export
#'
#' @importFrom graphics abline axis
#'
#' @examples \dontrun{mymaxlikg(theta=seq(0,1,length=10000))}
mymaxlikg <- function(lfun = "logbin2", theta) { # default log lik is a combination bin

  nth <- length(theta)  # nu. of values used in theta
  thmat <- matrix(theta, nr=nth, nc=1, byrow=TRUE) # Matrix of theta
  z <- apply(thmat, 1, lfun) # z holds the log lik values
  zmax <- max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta,exp(z),type="l", xlab="p", ylab="exp(y)") # plot of lik
  abline(v=theta[zmax],col="Blue")   #  verical line through max
  axis(3, theta[zmax], round(theta[zmax], 4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}
