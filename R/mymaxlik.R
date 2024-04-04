#' mymaxlik
#'
#' a general function that will use the grid approximation to find the maximum likelihood estimate of a one parameter likelihood
#'
#' @param lfun function to be used
#' @param x x values for the function
#' @param param parameter values to be sampled
#' @param ... pass additional options to the function
#'
#' @return a plot and a list
#' @export
#'
#' @importFrom graphics abline points axis
#'
#' @examples \dontrun{mymaxlik(lfun=logbin, x=c(3,4,5), param=seq(0,1,length=1000))}
mymaxlik <- function(lfun,x,param,...){
  np <- length(param)
  z <- outer(x,param,lfun)
  y <- apply(z,2,sum)
  plot(param,
       y,
       col="hotpink",
       type="l",
       lwd=2,
       ...)
  i <- max(which.max(y))
  abline(v=param[i],
         lwd=2,
         col="darkgreen")
  points(param[i],
         y[i],
         pch=19,
         cex=1.5,
         col="darkgreen")
  axis(3,
       param[i],
       round(param[i],2))
  ifelse(i-3 >= 1 & i+2 <= np,
         slope <-  (y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),
         slope <- "NA")
  return(invisible(list(i = i,
                        parami = param[i],
                        yi = y[i],
                        slope = slope)
  ))
}
