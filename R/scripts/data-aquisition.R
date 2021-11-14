get.raw.dataset <- function(){
  ids <- list(gini="1TOQiiau_U3Zmf7UckYdp0vX9r0Hk6OSj", 
              poverty="1NGhqxQ9yG4rHBjHabUsEB0k4NjCpiRPg", 
              education_boys="1XivWARyORXko0MAnYPSnf5IIu0s8S72p", 
              education_girls="1h43Tk-l0KlxTBMhcFpeSIYPTkU3jorld",
              babies_per_woman="1CAdLSh8oehf4CaUHZc1Ax48XtjGkuFub",
              contraception="174_oojvYBVSxf8QMY0wBCg5s_VKdsykV",
              income="1hfXAyRDE6HXr5CvGHv3fV0vph3ozDEwf", 
              suicide="1ythAEL4nSnQMB-QGwTFSmCsDvxgM_9Fp")
  x <- lapply(ids, function(x){
    data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", x))
    a <- grep("2000", colnames(data))
    b <- grep(format(Sys.Date(), format='%Y'), colnames(data))-1
    if( length(b)==0 ) b <- ncol(data)
    tibble::as_tibble( data[, c(1, a:b)], 
                       .name_repair = function(x) gsub(".*\\.", "", x) )
  })
  return(x)
}

get.melted.dataset <- function(data.list){
  lapply(names(data.list), function(x){
    data.list[[x]] %>% 
      dplyr::mutate_at(dplyr::vars(matches('^X[0-9].*$')), 
                       function(col) as.numeric(sub("k", "e3", col, fixed = TRUE))) %>%
      reshape2::melt(id=1, variable.name="year", value.name = x)
  }) %>% 
    purrr::reduce(dplyr::full_join) %>% 
    dplyr::mutate_at(c("year"), 
                     function(col) as.numeric(sub("X", "", col))) %>%
    filter_at(vars(-one_of(c("country", "year"))),
              any_vars(!is.na(.))) %>%
    dplyr::mutate_at(c("country"), as.factor)%>%
    tibble::as_tibble()
}

match.regions <- function(data.names, country.names){
  x <- sapply(data.names, function(x){
    country <- gsub("St\\.", "Saint", x)
    match <- which(country==gsub("\\:.*", "", country.names))
    if( length(match)!=0 ) return(country.names[match])
    country <- gsub("-", " ", country)
    country <- gsub("\\.|The |North |\\, Fed\\. Sts\\.| Leste", "", country)
    if( country=="Slovak Republic" ) country <- "Slovakia"
    else if( country=="United States" ) country <- "United States of America"
    else if( country=="Kyrgyz Republic" ) country <- "Kyrgyzstan"
    else if( country=="Virgin Islands (US)" ) country <- "United States Virgin Islands"
    else if( country=="Eswatini" ) country <- "Swaziland"
    else if( grepl("Congo", country) ){
      country <- unlist(strsplit(country, "\\, | "))
      country <- paste(c(country[-1], country[1]), collapse=".*")
      country <- paste0("^", country)
    }
    else if( grepl(", ", country)) country <- gsub(".*, ", "", country)
    country <- unlist(strsplit(country, " and "))
    country.names[unlist(lapply(country, function(x){
      grep(x, country.names)}))]
  })
  print(paste("Country not found:",names(x)[sapply(x, length)==0]))
  
  x %>% reshape2::melt() %>% 
    tibble::as_tibble()
}

append.regions <- function(data.melt){
  mapdata <- highcharter::get_data_from_map(highcharter::download_map_data("custom/world-lowres"))
  x <- match.regions(levels(data.melt$country), mapdata$name)
  names(x) <- c("name", "country")
  
  mapdata %>% 
    dplyr::select(name, continent, subregion) %>% 
    dplyr::right_join(x) %>%
    dplyr::right_join(data.melt) %>%
    dplyr::mutate_if(is.character, as.factor)
}

