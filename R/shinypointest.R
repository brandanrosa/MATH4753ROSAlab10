#' shinypointest
#'
#' An interactive shiny app which allows control over the alpha level and iterations
#'
#' @return a plot
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{shinypointest()}
shinypointest <- function() {
  runApp(system.file("shinypointest",
                     package = "MATH4753ROSAlab10"),
         launch.browser = TRUE)
}
