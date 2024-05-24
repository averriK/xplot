#' plot.ggplot2
#' @param plot.object ggplot2 object
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
#' @param legend.align character
#' @param legend.valign character
#' @param line.style character c("solid","dashed","dotted","dotdash","longdash","twodash")
#' @param plot.theme ggplot2 theme
#' @param point.style character c("circle","square","diamond","triangle","triangle-down")
#' @param plot.save boolean
#'
#' @return ggplot2 object
#' @export buildPlot.ggplot2
#' 
#' @examples
#' data(iris)
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' buildPlot.ggplot2(DT)
#' 
#' @import data.table data.table
#' @importFrom data.table as.data.table 
#' @importFrom grDevices hcl.colors
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_y_log10
#' @importFrom ggplot2 scale_x_log10
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_x_reverse
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom epoxy epoxy_html
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#'
#'

buildPlot.ggplot2 <- function(
    data,
    plot.object=NA,
    plot.title=NA,
    plot.subtitle=NA,
    plot.height=NA,
    plot.width=NA,
    xAxis.legend="X",
    yAxis.legend="Y",
    group.legend="ID",
    color.palette="viridis",
    plot.type="line",#line
    line.style="solid",
    line.size=1,
    point.size=2,
    point.style="circle",
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
    plot.save = FALSE,
    plot.theme=theme_bw()
    
){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  if(!all( c("ID","X","Y")%in% colnames(data))){
    stop("data must contain columns named ID, X, and Y")
  }
  
  DATA <- as.data.table(data)[,c("ID","X","Y")]
  
  if(all(is.na(plot.object)) ==TRUE){
    PLOT <- ggplot(
      data=DATA,
      aes(x=X,y=Y,group=ID,color=ID),
      aes_string(x = xAxis.legend, y = yAxis.legend, group = group.legend))
    
    COLORS <- grDevices::hcl.colors(n=length(unique(DATA$ID)),palette = color.palette)
    # Mapeo de estilos de puntos
    point.styles <- c(circle = 16, square = 15, diamond = 18, triangle = 17, "triangle-down" = 25)
    
    # Convertir estilo de punto a código numérico
    PS <- point.styles[[point.style]]
    if (is.null(PS)) {
      stop("Invalid point.style value. Choose from 'circle', 'square', 'diamond', 'triangle', 'triangle-down'")
    }
    PLOT <- PLOT +
      scale_color_manual(values = COLORS) +
      scale_fill_manual(values = COLORS)
    
    PLOT <- PLOT + plot.theme
    
    if(plot.type =="line"){
      PLOT <- PLOT +
        geom_line(size=line.size,linetype=line.style)
    }
    
    if(plot.type =="spline"){
      PLOT <- PLOT +
        geom_smooth(method = 'gam', formula = Y ~ s(X, bs = "cs"))
    }
    
    if(plot.type =="scatter"){
      PLOT <- PLOT +
        geom_point(size=point.size,shape=point.style)
    }
    
    
    # Configurar los límites de los ejes si se proporcionan
    if (!is.na(xAxis.min) || !is.na(xAxis.max)) {
      PLOT <- PLOT + xlim(c(xAxis.min, xAxis.max))
    }
    if (!is.na(yAxis.min) || !is.na(yAxis.max)) {
      PLOT <- PLOT + ylim(c(yAxis.min, yAxis.max))
    }
    
    if(yAxis.log==TRUE) {
      PLOT <- PLOT +
        scale_y_log10()
    }
    if(xAxis.log==TRUE) {
      PLOT <- PLOT +
        scale_x_log10()
    }
    
    if(yAxis.reverse==TRUE) {
      PLOT <- PLOT +
        scale_y_reverse()
    }
    
    if(xAxis.reverse==TRUE) {
      PLOT <- PLOT +
        scale_x_reverse()
    }
    
    if (!is.null(plot.title)) {
      PLOT <- PLOT + labs(subtitle = plot.title)
    }
    
    
    if (!is.null(plot.subtitle)) {
      PLOT <- PLOT + labs(subtitle = plot.subtitle)
    }
    PLOT <- PLOT + xlab(xAxis.legend) + ylab(yAxis.legend)
    
    # Configurar la leyenda
    if (!legend.show) {
      PLOT <- PLOT + theme(legend.position = "none")
    } else {
      PLOT <- PLOT + theme(legend.position = legend.align)
    }
    
    PLOT <- PLOT +
      ylab(yAxis.legend)+
      xlab(xAxis.legend)
    
    if(xAxis.label==FALSE){
      PLOT <- PLOT +
        theme(axis.title.x = element_blank())
    }
    
    
    if(yAxis.label==FALSE){
      PLOT <- PLOT +
        theme(axis.title.y = element_blank())
    }
  }
  
  if(all(is.na(plot.object)) ==FALSE){
    PLOT <- plot.object
  }
  


  return(PLOT)
}
