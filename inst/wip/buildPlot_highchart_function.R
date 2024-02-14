
library(highcharter)

buildPlot.highchart <- function(
  plot.data,
  plot.title = NULL,
  plot.subtitle = NULL,
  xAxis.legend = "X",
  yAxis.legend = "Y",
  group.legend = "ID",
  color.palette = "Viridis",
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
  
  PLOT <- highchart()  # Uso de PLOT como nombre de variable unificado
  class(PLOT) <- c("highcharter", class(PLOT))  # Asignar clase para identificar la función generadora

  if (plot.save) {
    hc_exporting(PLOT, enabled = TRUE)
    hc_save(PLOT, "plot_highchart.png")
  }
  
  return(PLOT)
}
