# CompareParamaterDistributions.r
# Plot the density of the group level paramaters with the stock level paramaters on top
# For several groups on the same plot to compare them

# groups            number of groups
# group.index  	    vector indicating which group each stock is in
# physical.names    names of physical variables
# stock.names       names of fish stocks
# stocks            number of fish stocks
# file.path         folder to save pdf in
# file.name         name of file to save pdf as
# x.min             x-axis minimum
# x.max             x-axis maximum
# usingMac          using a mac? for opending the plotting wondow

CompareParamaterDistributions <- function(groups, group.index, phys, physical.names, stock.names, stocks, file.path, file.name, x.min=NA, x.max=NA, usingMac=TRUE){
   
     #load library for nearest()
     library(GenKern) 
     
     #open plotting window
     if(usingMac==T) quartz() else windows()
     pdf(file=paste(file.path, file.name, ".pdf", sep=""), pointsize=12, width=8, height=5)
     par(mar=c(5,7,2,2))
     
     # set colors for density plots of group level paramater values, make them semi-transparent
     base.colors <- c('red', 'green', 'purple', 'grey', 'blue', 'yellow')
     density.colors <- rgb(t(col2rgb(base.colors)), alpha=50, maxColorValue=255)
     
     # get the number of stocks in each group
     group.totals <- tabulate(group.index)
     
     # list to hold colors for each stock by group
     stock.colors <- vector("list", length(groups))
     
     # first go through all the groups an assign colors
     for(j in 1:groups){ 
       # get stock group colors as shades of the group color
       colors.to.sample <- colors()[grep(base.colors[j], colors())]
       stock.colors[[j]] <- sample(colors.to.sample, size=group.totals[j])
     }
     
     # get the stock names in order of groups for plotting them in the legend
     stock.names.ordered <- NULL
     for(j in 1:groups){ 
       # get stock group colors as shades of the group color
       stock.names.ordered <- c(stock.names.ordered, stock.names[which(group.index==j)])
     }
     
     # for each physical variable, plot the group level paramater distributions with individual stock median 
     # and 95% CI overlaid
     for(i in 1:phys){ # for each physical variable
       
       # get x and axis limits based on the group densities
       den.all <- apply(beta.mu[, i, ], 2, density)
       smoothed.all <- lapply(X=den.all, FUN=function(x) ksmooth(x$x, x$y, kernel='normal', bandwidth=0.01))
       y.max <- 1.15*max(unlist(lapply(smoothed.all, FUN=function(x) max(x$y))))
       if(is.na(x.min)){
         x.min <- min(unlist(lapply(smoothed.all, FUN=function(x) min(x$x))))
       }
       if(is.na(x.max)){
         x.max <- max(unlist(lapply(smoothed.all, FUN=function(x) max(x$x))))
       }
      
       # open new plot for each physical variable
       plot(0, 0, type='n', main='', xlab='', ylab='',
            las=1, bty='n', xlim=c(x.min, x.max), ylim=c(0,max(y.max)), yaxs='i')
       mtext('Density', 2, line=5, cex=1.3)
       mtext(paste(physical.names[i], 'Parameter'), 1, line=2.5, cex=1.3)
       
       for(j in 1:groups){  
         #identify stocks in this group
         group.stocks <- which(group.index==j) # stock index
         num.stocks <- length(group.stocks) # number of stocks in group
  
         # plot group level parameter distribution
         xs <- c(smoothed.all[[j]]$x, smoothed.all[[j]]$x[1])
         ys <- c(smoothed.all[[j]]$y, smoothed.all[[j]]$y[1])
         polygon(xs, ys, col=density.colors[j])
     
         #for each stock plot the median and 95% CI on the distribution
         for(k in 1:num.stocks){ # for each stock
           
           #calculate values
           median <- median(beta[,group.stocks[k],i])
           lower <- quantile(beta[,group.stocks[k],i], 0.025)
           upper <- quantile(beta[,group.stocks[k],i], 0.975)
           
           #find closest location on the distribution to plot it the median at
           if(median < min(smoothed.all[[j]]$x) | median > max(smoothed.all[[j]]$x)){ # if the beta median is off the density plot
             x <- median
             y <- 0
           }else{
             x <- smoothed.all[[j]]$x[nearest(smoothed.all[[j]]$x, median)]
             y <- smoothed.all[[j]]$y[nearest(smoothed.all[[j]]$x, median)]	
           }
 
           # point at the median
           points(x, y, pch=19, col=stock.colors[[j]][k])
           
           # horizontal line showing 95% CI
           lines(c(lower, upper), c(y,y), col=stock.colors[[j]][k])
         }
   	}
    # add legend to label groups
    legend('topleft', legend=paste('Group', 1:groups), fill=density.colors[1:groups], bty='n', cex=1.5)
       
    # add verticle line at 0 so it is easy to idenify positive and negative
    abline(v=0, col='gray')
  }
     
  plot(1,1, type='n', xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
  legend('topleft', legend=stock.names.ordered, col=unlist(stock.colors), pch=19, bty='n', cex=1)
  dev.off()
  graphics.off()
     
}
 
