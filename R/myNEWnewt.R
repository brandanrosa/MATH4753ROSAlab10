#' myNEWnewt
#'
#' my take on `mynewt`, a function to find the derivative of f without having to input `fdash`
#'
#' @param x0 initial value
#' @param delta accuracy
#' @param f function
#' @param xlim limitations on x
#'
#' @return a plot and a list
#' @export
#'
#' @importFrom graphics abline points axis curve segments
#'
#' @examples \dontrun{myNEWnewt(x0 = 10, delta = 0.001, f = function(x) x^2 - 5*x + 6)}
myNEWnewt <- function(x0, delta=0.001, f, xlim = c(-x0,x0)) {

  d <- 100
  i <- 0
  x <- c()
  y <- c()
  x[1] <- x0
  y[1] <- f(x[1])

  h <- delta

  fdash <- function(x) {
    (f(x + (h/2)) - f(x - (h/2))) / h
  }

  while(d > delta & i < 1000){
    i <- i+1
    x[i+1] <- x[i] - f(x[i])/fdash(x[i])
    y[i+1] <- f(x[i+1])
    d <- abs(y[i])
  }

  curve(f(x),
        xlim = xlim,
        xaxt = "n",
        main = "The Return of the Newton-Raphson Algorithm")

  points(x,
         y,
         col = "red",
         pch = 19,
         cex = 1.5)

  axis(1,x,round(x,2),las=2)
  abline(h = 0,col = "red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="pink")

  list(x = x, y = y)
}
