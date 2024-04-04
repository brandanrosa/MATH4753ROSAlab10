#' mynewt
#'
#' a function which will find roots of a function as long as you are careful to start the algorithm with an appropriate initial value and have the derivative
#'
#' @param x0 initial value
#' @param delta accuracy
#' @param f function
#' @param fdash derivative of f
#' @param xlim limitations on x
#'
#' @return a plot and a list
#' @export
#'
#' @importFrom graphics abline points axis curve segments
#'
#' @examples \dontrun{mynewt(x0 = 10,delta = 0.000001,f = function(x) x^2-4,fdash = function(x) 2*x )}
mynewt=function(x0, delta=0.001, f, fdash, xlim = c(-x0,x0)) {

  d <- 100
  i <- 0
  x <- c()
  y <- c()
  x[1] <- x0
  y[1] <- f(x[1])

  while(d > delta & i < 1000){
    i <- i+1
    x[i+1] <- x[i] - f(x[i])/fdash(x[i])
    y[i+1] <- f(x[i+1])
    d <- abs(y[i])
  }

  curve(f(x),
        xlim = xlim,
        xaxt = "n",
        main = "Newton-Raphson Algorithm")

  points(x,
         y,
         col = "red",
         pch = 19,
         cex = 1.5)

  axis(1,x,round(x,2),las=2)
  abline(h = 0,col = "red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="pink")

  list(x = x,y = y)
}
