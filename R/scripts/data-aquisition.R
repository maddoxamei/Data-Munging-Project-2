get.dataset <- function(){
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
    tibble::as_tibble( data[, c(1, a:b)] ) 
  })
  return(x)
}