#' Title
#'
#' @param data data.frame
#' @param library character c("ggplot2","highcharter","plotly")
#' @param ... extra arguments
#'
#' @return plot object
#' @export xplot
#'
#' @examples
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' xplot(data=DT)
#' 
#' xplot(data=DT,library="highcharter")
#' 
xplot <- function(data, library = "ggplot2", ...) {
  # Verificar que 'data' sea un data.frame
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data.frame")
  }
  
  if (library == "ggplot2") {
    class(data) <- c("ggplot2", class(data))
    plot(data, ...)
  } else if (library == "highcharter") {
    class(data) <- c("highchart", class(data))
    plot(data, ...)
  } else if (library == "plotly") {
    class(data) <- c("plotly", class(data))
    plot(data, ...)
  } else {
    stop("unknown library")
  }
}
