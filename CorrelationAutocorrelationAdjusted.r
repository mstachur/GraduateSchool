CorrelationAutocorrelationAdjusted <- function(x,y){
    
    # calculate the correlation
    r <- cor(x, y, method='pearson', use='pairwise.complete.obs')
    
    # calculate the degrees of freedom corrected for autocorrelation
    Ns <- AutocorrelatedDegreesOfFreedom(x, y)
    N <- Ns[1]
    N.adjusted <- Ns[2]
    df <- N.adjusted-2
    
    # calculate the F statistic
    Fstat <- r^2*df/(1-r^2)
        
    # calculate the significance of the correlation
    sig <- 1 - pf(Fstat, 1, df)
    r <- formatC(r, format='f', digits=3)
    
    return(c(r, sig, N, N.adjusted))
}