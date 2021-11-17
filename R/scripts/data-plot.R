opts <- getOption("highcharter.lang")
opts$thousandsSep <- ","
options(highcharter.lang = opts)

plot.violin <- function(data, hc, var, group, subgroup, boxdata){
  ds <- NULL
  # var <- deparse(substitute(var))
  # group <- deparse(substitute(group))
  if( !is.factor(data[[group]]) ) data[[group]] <- as.factor(data[[group]])
  if( !is.factor(data[[subgroup]]) ) data[[subgroup]] <- as.factor(data[[subgroup]])
  
  sub.len <- length(levels(data[[subgroup]]))
  if( sub.len == 1 ) x2.inc <- 0
  else x2.inc <- (1/sub.len)/1.73#1.73
  offset <- -2/3*sub.len + 10/3 - sub.len%%2/3
  
  x.idx <- 0
  for(x in levels(data[[group]])){
    
    for( x2 in levels(data[[subgroup]])){
      subset <- subset(data, data[[group]]==x & data[[subgroup]]==x2, select=var)
      x2.idx <- match(x2, levels(data[[subgroup]]))-1
      
      if( sum(!is.na(subset[[var]])) < 2 ) next
      density <- density(subset[[var]], na.rm=T)
      idx <- x.idx - (x2.inc/offset) + (x2.idx*x2.inc)
      ds <- c(ds, list(list(data = cbind(density$y+idx,density$x), name=x, type="area", colorIndex=x2.idx, yAxis=x2.idx, zIndex=2),
                       list(data = cbind(-density$y+idx,density$x), name=x, type="area", colorIndex=x2.idx, yAxis=x2.idx, zIndex=2)))
      x2.idx <- x2.idx + 1
    }
    x.idx <- x.idx + 1
  }
  boxdata$zIndex <- 4
  hc %>% highcharter::hc_xAxis(type='category')%>%
    highcharter::hc_add_series_list(boxdata %>% mutate(yAxis=0:(nrow(boxdata)-1))) %>%
    highcharter::hc_add_series_list(ds)%>%
    highcharter::hc_plotOptions(area = list(fillOpacity=0.3,
                               lineWidth=0, 
                               linkedTo=':previous',
                               tooltip=list(
                                 headerFormat = '<b>{series.name}</b><br />',
                                 pointFormat='{series.yAxis.axisTitle.textStr}: {point.y}<br/>
                                              Density: {point.x}'),
                               scatter = list(tooltip=list(
                                 headerFormat = '<b>{point.x} Years</b><br />',
                                 pointFormat='{series.yAxis.axisTitle.textStr}: {point.y}'))))
}

hc.label <- function(hc, xlab, ylab,
                     title=NULL, subtitle=NULL){
  hc<- hc %>%
    highcharter::hc_xAxis( title = list(text = xlab)) %>%
    highcharter::hc_yAxis( title = list(text = ylab) ) %>%
    highcharter::hc_title(text=title ,align="center") %>%
    highcharter::hc_subtitle(text=subtitle,align="center") %>%
    highcharter::hc_tooltip(valueDecimals=2)
  return(hc)
}
