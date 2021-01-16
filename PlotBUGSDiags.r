 # PlotBUGSDiags.r
 # plot the diagnostis of a BUGS model, including Geweke statistic, effective sample size, Heidelberger-Welsh, paramater cross correlation
 # allOutsMCMC    an mcmc object
 # usingMac		  using a mac or pc? for opening plotting window
 
 PlotBUGSDiags <- function(allOutsMCMC, usingMac=TRUE){
   
     # set graphical paramaters
     par(mfrow=c(2,2), oma=c(0,2,0,0), mar=c(5,3.5,1,1))
     #Geweke statistic
     geweke <- geweke.diag(allOutsMCMC)
     #gewekeP <- 2*pnorm(-abs(geweke$z)) #for looking a p-values
     breaks <- seq(from=-6.37, to=6.37, by=.49) # get breaks to line up at significance lines
     hist(geweke$z, xlab='Geweke statistic', ylab='', main='', las=1, bty='l', breaks=breaks)
     legend('topleft', legend='', pch='a', bty='n')
     abline(v=1.96, lty=2) #plot lines at 95% significance levels
     abline(v=-1.96, lty=2)
     
     #Effective sample size
     effective.n <- effectiveSize(allOutsMCMC)
     hist(effective.n, xlab='Effective sample size', ylab='', main='', las=1, bty='l')
     legend('topleft', legend='', pch='b', bty='n')
     
     #Autocorrelation
     ac<- autocorr.diag(as.mcmc(allOutsMCMC), lags=1)
     hist(ac, xlab='Lag-1 Autocorrelation', ylab='', main='', las=1, bty='l')
     legend('topleft', legend='', pch='c', bty='n')
                      
     #Heidelberger-Welsh
     heidel <- heidel.diag(allOutsMCMC)
     heidel2 <- rep(NA, length(heidel[,1]))
     for(i in 1:length(heidel2)){
         if(heidel[i,1]==1){
             heidel2[i] <- 'Passed'
         }else heidel2[i] <- 'Failed'
     }
     x<-c(length(which(heidel[,1]==0)), length(which(heidel[,1]==1)))
     barplot(x, names.arg=c('Failed', 'Passed'), xlab='Heidelberger and Welch statistic', main='', las=1, bty='l')
     legend('topleft', legend='', pch='d', bty='n')
     mtext('Frequency', side=2, outer=T)
     
     # in new window, plot paramater cross correlations
     par(mfrow=c(1,1), oma=c(0,2,0,0), mar=c(5,3.5,1,1))
     color.palette <- colorRampPalette(c('darkblue', 'blue', 'white', 'orange', 'red'))
     crosscorr.plot(allOutsMCMC, col=color.palette(15), bty='l')
     mtext('Paramater Correlation', 1, line=3)
 }
 
# #####other diagnostics plotting
# par(ask=T, mfrow=c(4,4))
# cumuplot(allOutsMCMC)
# densplot(allOutsMCMC)
# gelman.rubin <- gelman.diag(jags.model1.MCMC)
# max(gelman.rubin$psrf[,1])
#  