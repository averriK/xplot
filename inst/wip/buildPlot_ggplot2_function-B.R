
library(ggplot2)

buildPlot.ggplot2 <- function(
  plot.data,
  plot.title = NULL,
  plot.subtitle = NULL,
  xAxis.legend = "X",
  yAxis.legend = "Y",
  group.legend = "ID",
  color.palette = "viridis",
  plot.type = "line",
  line.style = "solid",
  point.style = "circle",
  xAxis.log = FALSE,
  yAxis.log = FALSE,
  xAxis.reverse = FALSE,
  yAxis.reverse = FALSE,
  xAxis.max = NA,
  yAxis.max = NA,
  xAxis.min = NA,
  yAxis.min = NA,
  legend.show = TRUE,
  legend.align = "right",
  plot.save = FALSE
) {
  # Cuerpo de la función omitido para brevedad
  
  PLOT <- ggplot()  # Uso de PLOT como nombre de variable unificado
  class(PLOT) <- c("ggplot", class(PLOT))  # Asignar clase para identificar la función generadora

  if (plot.save) {
    ggsave("plot_ggplot2.png", PLOT)
  }
  
  return(PLOT)
}
