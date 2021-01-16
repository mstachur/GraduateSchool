 # PlotGroupParams2.r
 # Plot the density of the group level paramaters with the stock level paramaters on top
 # group.index		vector indicating which group each stock is in
 # group.num		group number to plot
 PlotGroupParams2 <- function(stocks, groups, names, phys, order, ecosystem, file.path=file.path, file.name='Compare groups', width=(3+phys*2), height=(1.5+stocks/5), usingMac=T){

   # open the plotting window
   if(usingMac==TRUE) quartz(w=width, h=height) else windows(w=width, h=height)
   pdf(paste(file.path, file.name, '.pdf', sep=''), w=width, h=height)

   # set graphical paramaters
   par(mfrow=c(1, phys+1), mar=c(3,0.5,1.5,0.5), oma=c(2,0,2.5,0.5))
   layout(matrix(seq(from=1, to=phys+1), 1, phys+1, byrow = T), widths=c(1.5, rep(1, phys)))
     
   plot(0, xlim=c(0,1), ylim=c(1, stocks + groups+1), xaxt='n', yaxt='n', bty='n', type='n')
   
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
   legend(-.05, stocks+groups+2.5, legend=c('Ecosystem-level median', 'Group-level median', 
                                         'Stock-level median', '95% credible interval'),
          pch=c(10,21,19,NA), lty=c(NA,NA,NA,1), horiz=T, bty='n', xpd=NA, cex=1.25, x.intersp = 0.1, xjust=0, yjust=0, seg.len=1.2)
   par(xpd=F)
     
   # for all the physical variables, plot the group level paramater distriubtion and the individual stock 
   # mean and 95% CI on that distribution
   for(i in 1:phys){
     
     # Calculate the 95% CI for all the stocks and physical variables to get the x limits for plotting
     median <- array(NA, stocks+groups+1)
     lower <- array(NA, stocks+groups+1)
     upper <- array(NA, stocks+groups+1)
     
     # stock-level parameter
     for(j in 1:stocks){
       median[j] <- median(beta[,j,i])
       lower[j] <- quantile(beta[,j,i], 0.025)
       upper[j] <- quantile(beta[,j,i], 0.975)
     }
     
     # group-level parameter
     for(j in 1:groups){
       median[j+stocks] <- median(beta.mu[, i, j])
       lower[j+stocks] <- quantile(beta.mu[, i, j], 0.025)
       upper[j+stocks] <- quantile(beta.mu[, i, j], 0.975)
     }
     
     # ecosystem-level parameter
     median[j+stocks+1] <- median(beta.mu.mu.adj[, i])
     lower[j+stocks+1] <- quantile(beta.mu.mu.adj[, i], 0.025)
     upper[j+stocks+1] <- quantile(beta.mu.mu.adj[, i], 0.975)
     
     xlim <- max(c(abs(lower), upper))
     
     plot(0, xlim=c(-xlim, xlim), ylim=c(1, stocks + groups + 1), yaxt='n', bty='n', type='n', cex.axis=1)
     title(paste('PC',i, sep=''), line=0, font.main=1, cex.main=1.4, adj=0) # title with PC number
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
     abline(v=0, lty=2)
     
     # save the 95% CI
     results <- cbind(lower, median, upper)
     row.names(results) <- c(names, group.names, ecosystem)
     write.csv(results, paste(file.path, file.name, 'PC',i, '.csv', sep=''))
   }
    mtext('Parameter', 1, line=0, outer=T, cex=1)
   dev.off()
   graphics.off()
  
}
     