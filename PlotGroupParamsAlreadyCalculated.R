 # PlotGroupParamsAlreadyCalculated.r
 # Plot the median and 95% credible intervals for the stock, group, and ecosystem level parameters
 # Median and 95% credible intervals already calculated previously
 # group.index		vector indicating which group each stock is in
 # group.num		group number to plot

PlotGroupParamsAlreadyCalculated <- function(stocks, groups, names, phys, order, ecosystem, file.path=file.path, file.name='Compare groups', width=(3+phys*2), height=(1.5+stocks/5), usingMac=T, title.lines=rep(-2, phys)){

   # open the plotting window
   if(usingMac==TRUE) quartz(w=width, h=height) else windows(w=width, h=height)
   pdf(paste(file.path, file.name, '.pdf', sep=''), w=width, h=height)

   # set graphical paramaters
   par(mfrow=c(1, phys+1), mar=c(3,0.2,1.5,0.2), oma=c(2,0,2.5,0.5))
   layout(matrix(seq(from=1, to=phys+1), 1, phys+1, byrow = T), widths=c(1.5, rep(1, phys)))
     
   plot(0, xlim=c(0,1), ylim=c(1, stocks + groups+2.5), xaxt='n', yaxt='n', bty='n', type='n')
   
   if(ecosystem=='EBS'){
     group.names <-c('Cross-shelf transport', 'Retention', 'Parental investment')
#      group.size <- c(4,6,4)
     group.size <- c(2,8,4)
     ecosystem.name <- 'Eastern Bering Sea & Aleutian Islands'
   }else if(ecosystem=='GOA'){
     group.names <-c('Cross-shelf transport', 'Retention', 'Coastal', 'Parental investment')
     group.size <- c(5,3,2,4)
     ecosystem.name <- 'Gulf of Alaska'
   }else if(ecosystem=='CC'){
     group.names <-c('Cross-shelf transport', 'Moderate upwelling')
     group.size <- c(11,12,1)
     ecosystem.name <- 'California Current'
   }
   
   # plot the ecosystem name
   text(-.05, groups+stocks+1, ecosystem.name, pos=4, cex=1.25, xpd=NA)
   
   # plot the group labels
   new.points <- NULL
   y <- groups+stocks
   for(i in 1:groups){
     text(0, y, group.names[i], pos=4, cex=1.25)
     y <- y-group.size[i]-1
     new.points <- c(new.points, y)
   }
   
   # plot the stock names
   y <- groups+stocks-1
   for(i in 1:stocks){
     if(y %in% new.points) y <- y-1
     text(0, y, names[order(order)[i]], pos=4, offset=2, cex=1.25)
     y <- y-1
   }
   
   # plot a legend 
   par(xpd=NA)
   legend(-.05, stocks+groups+4, legend=c('Ecosystem-level median', 'Group-level median', 
                                         'Stock-level median', '95% credible interval'),
          pch=c(10,21,19,NA), lty=c(NA,NA,NA,1), horiz=T, bty='n', xpd=NA, cex=1.25, x.intersp = 0.1, xjust=0, yjust=0, seg.len=1.2)
   par(xpd=F)
     
   # for all the physical variables, plot the group level paramater distriubtion and the individual stock 
   # mean and 95% CI on that distribution
   for(i in 1:phys){
     
     # get the median and 95% credible intervals
     intervals <- read.csv(paste(file.path, 'Parameter distributionsPC', i, '.csv', sep=''), header=T, row.names=1)
     median <- intervals$median
     lower <- intervals$lower
     upper <- intervals$upper
     
     # calculate the appropriate x-axis limit
     xlim <- max(c(abs(lower), upper))
     
     plot(0, xlim=c(-xlim, xlim), ylim=c(1, stocks + groups + 2.5), yaxt='n', bty='n', type='n', cex.axis=1)
     title <- paste('PC',i, '\n', PClabels[i], sep='')
     title(title, line=title.lines[i], font.main=1, cex.main=1.25, adj=0) # title with PC number
     #        title(letters[i], line=0, font.main=1, cex.main=1.5, adj=0) # title with letter
     
     # plot the ecosystem-level parameter
     y <- groups+stocks+1
     lines(c(lower[y], upper[y]), c(y,y))
     points(median[y], y, pch=10, bg='white', cex=1.5)
     
     # plot the group level median and 95% credible interval
     y <- groups+stocks
     for(j in 1:groups){
       lines(c(lower[j+stocks], upper[j+stocks]), c(y,y))
       points(median[j+stocks], y, pch=21, bg='white', cex=1.5)
       y <- y-group.size[j]-1
     }
     
     # plot the stocks median and 95% credible interval
     y <- groups+stocks-1
     for(j in 1:stocks){
       if(y %in% new.points) y <- y-1
       points(median[order(order)[j]], y, pch=21, bg='black', cex=1.5)
       lines(c(lower[order(order)[j]], upper[order(order)[j]]), c(y,y))
       y <- y-1
     }
#      abline(v=0, lty=2)
     lines(x=c(0, 0), y=c(0, stocks+groups+1.25), lty=2)
     
   }
   
   mtext('Parameter', 1, line=0, outer=T, cex=1)
   dev.off()
   graphics.off()
  
}
     