# PlotBUGSFitScatterplot.r
# plots         the observed and predicted logR for each stock
# rec           observed recruitment data
# rec.save      predicted recruitment data from BUGS, array of size [runs, years, stocks
# names   names of fish stocks
# plot.rows     number of rows in plot window
# plot.cols     number of cols in plot window
# usingMac      using Mac or PC? need to know for opening plot window

PlotBUGSFitScatterplot <- function(rec, rec.save, names, stocks, years, order, plot.rows=5, plot.cols=3, plot.height=10, file.path='', file.name='Model fit scatterplot', ecosystem, usingMac=TRUE){
  
  # get the correlations to put on the plot
  correlations <- ModelFitCorrelationByStock(stocks, years, names, rec.original, return.result=T)
  correlations <- formatC(as.numeric(correlations[,1]), format='f', digits=2)
  
  # open the plotting window
  if(usingMac==TRUE) quartz(w=9, h=plot.height) else windows(w=9, h=plot.height)
  if(ecosystem=='CC') pdf(paste(file.path, file.name, ' first page.pdf', sep=''), w=9, h=plot.height) else pdf(paste(file.path, file.name, '.pdf', sep=''), w=9, h=plot.height)
  
  # number of stocks
  num.stocks <- ncol(rec) 
  
  # set graphical paramaters
  par(mfrow=c(plot.rows,plot.cols), mar=c(1,3,2,0), oma=c(3, 2.2, 0,1))
  
  # keep a count of how many plots
  plots <- 0
  
  for(i in 1:stocks){
    plots <- plots+1
    
    col <- which(order==i)
    #observed rec
    observed <- rec[,col]
    names(observed) <- row.names(rec)
    
    #only plot predicted values in years that there is an observed value
    years.to.plot <- which(is.na(observed)==F) 
    
    #calculated the median prediced and 95% CI for all years for this stock
    predicted.median <- rep(NA, years)
    predicted.lower <- rep(NA, years)
    predicted.upper <- rep(NA, years)
    for(y in 1:years){
      predicted <- rec.save[,y,col]
      predicted.median[y] <- median(predicted)
      predicted.lower[y] <- quantile(predicted, 0.025)
      predicted.upper[y] <- quantile(predicted, 0.975)
    }
    
    plot(observed[years.to.plot], predicted.median[years.to.plot], type='p', pch=19, cex=.5, main='', 
         bty='n', xlab='', ylab='', las=1, bty='l') # plot observed vs predicted recruitment
    title(paste(names[col], ", ", correlations[col], sep=''), line=0.2, font.main=1, cex.main=1.15, adj=0) #title is stock name and correlation between observed and median predicted recruitment
    
    if(plots==(plot.rows*plot.cols) | plots==stocks){
      mtext('Observed stock-recruitment residuals', side=1, outer=T, line=1.5) #label xaxis
      mtext('Predicted stock-recruitment residuals', side=2, outer=T, line=0.5) #label yaxis
    }
  }
  dev.off()
  graphics.off()
  
  # only for CC, plot second page of the correct size
  if(ecosystem=='CC'){
    # open the second plotting window
    if(usingMac==TRUE) quartz(w=9, h=5.65) else windows(w=9, h=5.65)
    pdf(paste(file.path, file.name, ' second page.pdf', sep=''), w=9, h=5.65)
    
    # set graphical paramaters
    par(mfrow=c(3,plot.cols), mar=c(1,3,2,0), oma=c(3, 2.2, 0,1))
    
    plots <- plot.rows*plot.cols
    for(i in (plot.rows*plot.cols+1):stocks){
      plots <- plots+1
      col <- which(order==i)
      
      #observed rec
      observed <- rec[,col]
      names(observed) <- row.names(rec)
      
      #only plot predicted values in years that there is an observed value
      years.to.plot <- which(is.na(observed)==F) 
      
      #calculated the median prediced and 95% CI for all years for this stock
      predicted.median <- rep(NA, years)
      predicted.lower <- rep(NA, years)
      predicted.upper <- rep(NA, years)
      for(y in 1:years){
        predicted <- rec.save[,y,col]
        predicted.median[y] <- median(predicted)
        predicted.lower[y] <- quantile(predicted, 0.025)
        predicted.upper[y] <- quantile(predicted, 0.975)
      }
      
      plot(observed[years.to.plot], predicted.median[years.to.plot], type='p', pch=19, cex=.5, main='', 
           bty='n', xlab='', ylab='', las=1, bty='l') # plot observed vs predicted recruitment
      title(paste(names[col], ", ", correlations[col], sep=''), line=0.2, font.main=1, cex.main=1.15, adj=0) #title is stock name and correlation between observed and median predicted recruitment
      
      if(plots==(plot.rows*plot.cols) | plots==stocks){
        mtext('Observed stock-recruitment residuals', side=1, outer=T, line=1.5) #label xaxis
        mtext('Predicted stock-recruitment residuals', side=2, outer=T, line=0.5) #label yaxis
      }
    }
    dev.off()
    graphics.off()
  }
     
}
 

 
