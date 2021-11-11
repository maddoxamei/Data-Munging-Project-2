get.raw.dataset <- function(){
  ids <- list(gini="1TOQiiau_U3Zmf7UckYdp0vX9r0Hk6OSj", 
              poverty="1NGhqxQ9yG4rHBjHabUsEB0k4NjCpiRPg", 
              education_boys="1XivWARyORXko0MAnYPSnf5IIu0s8S72p", 
              education_girls="1h43Tk-l0KlxTBMhcFpeSIYPTkU3jorld",
              babies_per_woman="1CAdLSh8oehf4CaUHZc1Ax48XtjGkuFub",
              infant_mortality="1pfT85eqVajo2g7qma3ivt73g7VIuSf0Y",
              child_mortality="1EMRPXPjymLdShvCRn8LYBt5ckvt5iBfi",
              maternal_mortality="1QQaI_YsMa68jnf3Tpcle9qanEH8nOZ1-",
              income_per_person="1hfXAyRDE6HXr5CvGHv3fV0vph3ozDEwf")
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
    dplyr::mutate_at(c("country"), as.factor)%>%
    tibble::as_tibble()
}

country.polygons <- function(){
  x <- sapply(data$gini$country, function(x){
    country <- which(x==gsub("\\:.*", "", world.map$names))
    if( length(country)!=0 ) return(world.map$names[country])
    
    if( x=="United States" ) country <- "USA"
    else if( x=="United Kingdom" ) country <- "UK"
    else{
      country <- gsub("St\\.", "Saint", x)
      country <- gsub("\\.|the |Fed\\. Sts\\.", "", country)
      if( grepl(", ", country) ){
        country <- unlist(strsplit(country, "\\, | "))
        country <- paste(c(country[-1], country[1]), collapse=".*")
      }
      country <- unlist(strsplit(country, " and "))
    }
    world.map$names[unlist(lapply(country, function(x){
      grep(paste0("^", x), 
           gsub("\\:.*", "", world.map$names))}))]
  })
  hong.kong <- grep("Hong Kong", x$China)
  x$`Hong Kong, China` <- x$China[hong.kong]
  x$China <- x$China[-hong.kong]
  print(paste("Country not found:",names(x)[sapply(x, length)==0]))
  return(x)
}
