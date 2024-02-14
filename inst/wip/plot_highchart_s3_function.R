
plot.highchart <- function(x, ...) {
  UseMethod("plot.highchart", x)
}

plot.highchart.default <- function(x, ...) {
  # Aquí debería ir la implementación de plot.highchart utilizando highcharter, similar a buildPlot.highchart
  # La lógica específica se ha omitido para brevedad
}

# Registrar el método S3
if (!exists("plot.highchart")) {
  setMethodS3("plot", "highchart", plot.highchart.default)
}
