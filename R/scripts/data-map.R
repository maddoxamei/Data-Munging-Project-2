country.polygons <- function(data.names, country.names){
  x <- sapply(data.names, function(x){
    country <- which(x==gsub("\\:.*", "", country.names))
    if( length(country)!=0 ) return(country.names[country])
    
    if( x=="United States" ) country <- "USA"
    else if( x=="United Kingdom" ) country <- "UK"
    else if( x=="Slovak Republic" ) country <- "Slovakia"
    else if( x=="Kyrgyz Republic" ) country <- "Kyrgyzstan"
    else if( x=="British Virgin Islands") country <- "Virgin Islands, British"
    else if( x=="Virgin Islands (U.S.)") country <- "Virgin Islands, US"
    else{
      country <- gsub("St\\.", "Saint", x)
      country <- gsub("\\.|the |Fed\\. Sts\\.", "", country)
      if( grepl(", ", country) ){
        country <- unlist(strsplit(country, "\\, | "))
        country <- paste(c(country[-1], country[1]), collapse=".*")
      }
      country <- unlist(strsplit(country, " and "))
    }
    country.names[unlist(lapply(country, function(x){
      grep(paste0("^", x), 
           gsub("\\:.*", "", country.names))}))]
  })
  hong.kong <- grep("Hong Kong", x$China)
  x$`Hong Kong, China` <- x$China[hong.kong]
  x$China <- x$China[-hong.kong]
  # print(paste("Country not found:",names(x)[sapply(x, length)==0]))
  
  x %>% reshape2::melt() %>% 
    tibble::as_tibble()
}

establish.regions <- function(data.names){
  world.map <- maps::map("world", fill = TRUE, plot = FALSE)
  x <- country.polygons(data.names, world.map$names)
  z <- maptools::map2SpatialPolygons(world.map, IDs=world.map$names)
  # q <- sapply(names(z), function(wn){
  #   v <- names(which(sapply(x, function(x){wn %in% x}))) 
  #   if(length(v)==0) return(NA) 
  #   v})
  # w <- sp::SpatialPolygonsDataFrame(z, data.frame(country=q, row.names=names(z)))
  q <- names(z) %>% 
    tibble::as_tibble() %>% 
    dplyr::left_join(x) %>% 
    dplyr::rename(country = L1) %>% 
    tibble::column_to_rownames(var = "value")
  w <- sp::SpatialPolygonsDataFrame(z, q)
  w@data %<>% 
    tibble::rownames_to_column(var = "ID") %>% 
    dplyr::left_join(data.melt.avg) %>% 
    tibble::as_tibble() %>% 
    tibble::column_to_rownames(var = "ID")
  return(w)
}

map.palettes <- function(domain, palette="YlGnBu", reverse.palette = F, n.palettes=11, nquantiles=4, domain.stretch=F){
  
  if( nquantiles>11 ) nquantiles <- 11
  pal.quan <- leaflet::colorQuantile(
    palette = RColorBrewer::brewer.pal(nquantiles, palette),
    domain = abs(domain),
    n = nquantiles,
    reverse = reverse.palette)
  
  if( domain.stretch ) domain <- -ceiling(max(abs(domain), na.rm=T)):ceiling(max(abs(domain), na.rm=T))
  pal.range <- leaflet::colorNumeric(
    palette = RColorBrewer::brewer.pal(n.palettes, palette),
    domain = domain,
    reverse = reverse.palette)
  
  return(list(range=pal.range, quan=pal.quan))
}

data.map <- function(regions, variable, ...){
  values <- regions@data[[variable]]
  pals <- map.palettes(values, ...)
  
  leaflet::leaflet(regions)%>%
    leaflet::addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, color = ~pals$range(values), label = ~paste0(country,": ", values), group="Range")%>%
    leaflet::addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, color = ~pals$quan(abs(values)), label = ~paste0(country,": ", abs(values)), group="Quantiles")%>%
    leaflet::addLegend(position = "bottomright", pal = pals$range, values = ~values, title = "Range", group = "Range", layerId = "Range")%>%
    leaflet::addLegend(position = "bottomright", pal = pals$quan, values = ~abs(values), title = "Quantiles", group = "Quantiles", layerId = "Quantiles")%>%
    leaflet::addLayersControl(baseGroups = c("Range", "Quantiles"), options = leaflet::layersControlOptions(collapsed = FALSE))%>%
    htmlwidgets::onRender("
    function() { 
      var map = this;
      var legends = map.controls._controlsById;
      function addActualLegend() {
         var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
         $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
         map.addControl(legends[sel]);
      }
      $('.leaflet-control-layers-base').on('click', addActualLegend);
      addActualLegend();
   }") %>%
    leaflet::addEasyButton(leaflet::easyButton(
      icon="fa-globe", title="Zoom to World View",
      onClick=htmlwidgets::JS("function(btn, map){ map.setZoom(1); }"))) %>%
    leaflet::addMiniMap(tiles = "CartoDB.PositronNoLabels", toggleDisplay = T)
}

