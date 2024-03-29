% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.highchart.R
\name{plot.highchart}
\alias{plot.highchart}
\title{plot.highchart.default}
\usage{
\method{plot}{highchart}(
  data,
  plot.object = NA,
  plot.title = NA,
  plot.subtitle = NA,
  plot.height = NA,
  plot.width = NA,
  xAxis.legend = "X",
  yAxis.legend = "Y",
  group.legend = "ID",
  color.palette = "Viridis",
  plot.type = "line",
  line.style = "Solid",
  point.style = "circle",
  line.size = 1,
  point.size = 2,
  xAxis.log = FALSE,
  yAxis.log = FALSE,
  xAxis.reverse = FALSE,
  yAxis.reverse = FALSE,
  xAxis.max = NA,
  yAxis.max = NA,
  xAxis.min = NA,
  yAxis.min = NA,
  xAxis.label = TRUE,
  yAxis.label = TRUE,
  legend.layout = "horizontal",
  legend.align = "right",
  legend.valign = "top",
  legend.show = TRUE,
  plot.theme = hc_theme_hcrt(),
  plot.save = FALSE
)
}
\arguments{
\item{data}{data.frame}

\item{plot.object}{highcharter object}

\item{plot.title}{character}

\item{plot.subtitle}{character}

\item{plot.height}{numeric}

\item{plot.width}{numeric}

\item{xAxis.legend}{character}

\item{yAxis.legend}{character}

\item{group.legend}{character}

\item{color.palette}{character}

\item{plot.type}{character c("line","spline","point","column","bar")}

\item{line.style}{character c("Solid","Dashed","Dotted", "DashDot","LongDash","LongDashDot","LongDashDotDot")}

\item{point.style}{character c("circle","square","diamond","triangle","triangle-down")}

\item{line.size}{numeric}

\item{point.size}{numeric}

\item{xAxis.log}{boolean}

\item{yAxis.log}{boolean}

\item{xAxis.reverse}{boolean}

\item{yAxis.reverse}{boolean}

\item{xAxis.max}{numeric}

\item{yAxis.max}{numeric}

\item{xAxis.min}{numeric}

\item{yAxis.min}{numeric}

\item{xAxis.label}{boolean}

\item{yAxis.label}{boolean}

\item{legend.layout}{character}

\item{legend.align}{character c("center", "left", "right")}

\item{legend.valign}{character c("top", "middle", "bottom")}

\item{legend.show}{boolean}

\item{plot.theme}{highcharter object}

\item{plot.save}{boolean}
}
\value{
highcharter object
}
\description{
Title
}
\examples{
data(iris)
DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
plot.highchart(data=DT,plot.type="scatter")

}
