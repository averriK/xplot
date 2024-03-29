#' plot.highchart.default
#'
#' Title
#' @param plot.object highcharter object
#' @param plot.title character
#' @param plot.subtitle character
#' @param data data.frame
#' @param plot.height numeric
#' @param plot.width numeric
#' @param legend.show boolean
#' @param xAxis.legend character
#' @param yAxis.legend character
#' @param group.legend character
#' @param color.palette character
#' @param plot.type character c("line","spline","point","column","bar")
#' @param xAxis.log boolean
#' @param yAxis.log boolean
#' @param xAxis.reverse boolean
#' @param yAxis.reverse boolean
#' @param line.size numeric
#' @param point.size numeric
#' @param xAxis.max numeric
#' @param yAxis.max numeric
#' @param xAxis.min numeric
#' @param yAxis.min numeric
#' @param xAxis.label boolean
#' @param yAxis.label boolean
#' @param legend.layout character
#' @param legend.align character c("center", "left", "right")
#' @param legend.valign character c("top", "middle", "bottom")
#' @param line.style character c("Solid","Dashed","Dotted", "DashDot","LongDash","LongDashDot","LongDashDotDot")
#' @param plot.theme highcharter object
#' @param point.style character c("circle","square","diamond","triangle","triangle-down")
#' @param plot.save boolean

#' @return highcharter object
#' @export plot.highchart
#' 
#' @examples
#' data(iris)
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' plot.highchart(data=DT,plot.type="scatter")
#'
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table 
#' @importFrom grDevices hcl.colors
#' @importFrom highcharter hc_add_series
#' @importFrom highcharter hc_add_theme
#' @importFrom highcharter hc_colors
#' @importFrom highcharter highchart
#' @importFrom highcharter hc_legend
#' @importFrom highcharter hcaes
#' @importFrom highcharter hc_theme_hcrt
#' @importFrom highcharter hc_xAxis
#' @importFrom highcharter hc_yAxis
#' @importFrom highcharter hc_tooltip
#' @importFrom highcharter hc_chart
#' @importFrom highcharter hc_plotOptions
#' @importFrom highcharter hc_size
#' @importFrom highcharter hc_tooltip
#' @importFrom highcharter hc_title
#' @importFrom highcharter hc_subtitle
#' @importFrom epoxy epoxy_html
#'

plot.highchart <- function(
    data,
    plot.object=NA,
    plot.title=NA,
    plot.subtitle=NA,
    plot.height=NA,
    plot.width=NA,
    xAxis.legend="X",
    yAxis.legend="Y",
    group.legend="ID",
    color.palette="Viridis",
    plot.type="line",#line
    line.style="Solid",
    point.style="circle",
    line.size=1,
    point.size=2,
    xAxis.log=FALSE,
    yAxis.log=FALSE,
    xAxis.reverse=FALSE,
    yAxis.reverse=FALSE,
    xAxis.max=NA,
    yAxis.max=NA,
    xAxis.min=NA,
    yAxis.min=NA,
    xAxis.label=TRUE,
    yAxis.label=TRUE,
    legend.layout="horizontal",
    legend.align="right", #c("center", "left", "right")
    legend.valign="top", #c("top", "middle", "bottom")
    legend.show=TRUE,
    plot.theme=hc_theme_hcrt(),
    plot.save = FALSE

){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  #
  if(!all( c("ID","X","Y")%in% colnames(data))){
    stop("data must contain columns named ID, X, and Y")
  }
  #
  DATA <- as.data.table(data)[,c("ID","X","Y")]
  
  TIP = "{{group.legend}}:{point.series.name}<br> {{xAxis.legend}}={point.x}<br> {{yAxis.legend}}={point.y}" |> epoxy::epoxy_html()
  COLORS <- grDevices::hcl.colors(n=max(2,length(unique(DATA$ID))),palette = color.palette)
  
  if(all(is.na(plot.object)) ==TRUE){
    PLOT <- highchart()
    
    # c("line","spline","point","column","bar")
    if(plot.type %in% c("line","spline")){
      PLOT <- PLOT |>
        hc_add_series(
          data=DATA,# main curve
          type=plot.type,
          dashStyle = line.style,
          hcaes(x=X,y=Y,group=ID,color=ID))
    }
    
    #
    if(plot.type=="scatter"){
      
      PLOT <- PLOT |>
        hc_add_series(
          data=DATA,# main curve
          type="scatter",
          marker=list(symbol=point.style),
          hcaes(x=X,y=Y,group=ID))
      
    }
    
    PLOT <- PLOT |>
      hc_add_theme(hc_thm = plot.theme) |>
      
      hc_yAxis(
        labels = list(enabled=yAxis.label),
        title= list(text=yAxis.legend),
        minorTickInterval = "auto",
        minorGridLineDashStyle = "Dot",
        showFirstLabel = FALSE,
        showLastLabel = TRUE) |>
      
      hc_xAxis(
        labels = list(enabled=xAxis.label),
        title= list(text=xAxis.legend),
        minorTickInterval = "auto",
        minorGridLineDashStyle = "Dot",
        showFirstLabel = TRUE,
        showLastLabel = TRUE) |>
      
      hc_colors(
        colors = COLORS) |>
      
      hc_tooltip(
        sort = FALSE,
        split=FALSE,
        crosshairs = TRUE,
        pointFormat = TIP) |>
      
      hc_legend(
        enabled = legend.show,
        align = legend.align,
        verticalAlign = legend.valign,
        layout = legend.layout) |>
      
      hc_chart(
        style=list(fontFamily = "Helvetica"))
    
    if(!is.na(plot.title)){
      PLOT <- PLOT |>
        hc_title(text=plot.title, fontSize = "24px")
      
    }
    
    if(!is.na(plot.subtitle)){
      PLOT <- PLOT |>
        hc_subtitle(text=plot.subtitle,  fontSize = "18px")
    }
    
    if(!is.na(xAxis.max)){
      PLOT <- PLOT |>
        hc_xAxis(max = xAxis.max)
    }
    if(!is.na(yAxis.max)){
      PLOT <- PLOT |>
        hc_yAxis(max = yAxis.max)
    }
    
    if(!is.na(xAxis.min)){
      PLOT <- PLOT |>
        hc_xAxis(min = xAxis.min)
    }
    if(!is.na(yAxis.min)){
      PLOT <- PLOT |>
        hc_yAxis(min = yAxis.min)
    }
    if(yAxis.log==TRUE) {
      PLOT <- PLOT |>
        hc_yAxis(type = "logarithmic")
    }
    if(xAxis.log==TRUE) {
      PLOT <- PLOT |>
        hc_xAxis(type = "logarithmic")
    }
    
    if(xAxis.reverse==TRUE) {
      PLOT <- PLOT |>
        hc_xAxis(reversed=TRUE)
    }
    
    if(yAxis.reverse==TRUE) {
      PLOT <- PLOT |>
        hc_yAxis(reversed=TRUE)
    }
    
    if(!is.na(plot.height) & is.na(plot.width)){
      PLOT <- PLOT |> hc_size( height = plot.height)
    }
    
    
    if(!is.na(plot.width) & is.na(plot.height)){
      PLOT <- PLOT |> hc_size( width=plot.width)
    }
    
    if(!is.na(plot.width) & !is.na(plot.height)){
      PLOT <- PLOT |> hc_size( width=plot.width,height = plot.height)
    }
  }
  
  if(all(is.na(plot.object)) ==FALSE){
    PLOT <- plot.object
    
    
    
    if(plot.type %in% c("line","spline")){
      PLOT <- PLOT |>
        hc_add_series(
          data=DATA,# main curve
          type=plot.type,
          dashStyle = line.style,
          hcaes(x=X,y=Y,group=ID))
    }
    
    #
    if(plot.type %in% c("point","scatter")){
      
      PLOT <- PLOT |>
        hc_add_series(
          data=DATA,# main curve
          type="scatter",
          marker=list(symbol=point.style),
          hcaes(x=X,y=Y,group=ID))
      
    }
  }


  return(PLOT)
}

