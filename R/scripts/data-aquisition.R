get.dataset <- function(){
  ids <- list(gini="1TOQiiau_U3Zmf7UckYdp0vX9r0Hk6OSj", poverty="1NGhqxQ9yG4rHBjHabUsEB0k4NjCpiRPg")
  x <- lapply(ids, function(x){
    data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", x))
    a <- grep("2000", colnames(data))
    b <- grep(format(Sys.Date(), format='%Y'), colnames(data))-1
    if( length(b)==0 ) b <- ncol(data)
    data[, a:b]
  })
  return(x)
}

