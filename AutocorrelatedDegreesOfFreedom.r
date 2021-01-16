# Calculate the autocorrelation adjusted degrees of freedom for two time series 
# To be used in calculation of correlation significance
AutocorrelatedDegreesOfFreedom <- function(x, y){
  
  # only use years with data for both time series
  no.nas <- na.omit(cbind(x,y))
  x <- no.nas[,1]
  y <- no.nas[,2]
  N <- length(x)
  
  # calculate autocorrelation
  years.autocorrelation <- round(N/5)
  total <- 0
  for(i in 1:years.autocorrelation){
    autocorr.x <- acf(x, plot=F)$acf[i+1]
#     cor(x[1:(N-i)], x[(i+1):N], method='pearson')
    autocorr.y <- acf(y, plot=F)$acf[i+1]
    total <- total+(autocorr.x*autocorr.y)
  }
  
  # calculate the adjusted degrees of freedom
  N.adjusted <- ((1/N)+((2/N)*total))^-1
  return(c(N, N.adjusted))
}