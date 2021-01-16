# PlotGroupParams2.r
# Plot the density of the group level paramaters with the stock level paramaters on top
# group.index		vector indicating which group each stock is in
# group.num		group number to plot
PlotGroupParamsPaperGrid <- function(stocks, groups, names, phys, order, ecosystem, file.path=file.path, file.name='Compare groups', grid.width=3, grid.height=2, width=(3+grid.width*2), height=(1.5+grid.height*stocks/5), usingMac=T, title.lines=rep(-1.7, phys)){

   # open the plotting window
   if(usingMac==TRUE) quartz(w=width, h=height) else windows(w=width, h=height)
   pdf(paste(file.path, file.name, '.pdf', sep=''), w=width, h=height)

   # set graphical paramaters
   par(mfrow=c(grid.height, grid.width+1), mar=c(2.5,0.2,2,0.2), oma=c(2,0,0,0.5))
   layout(matrix(seq(from=1, to=(grid.height*(grid.width+1))), nrow=grid.height, ncol=grid.width+1, byrow = T), widths=c(1.5, rep(1, grid.width)))
   
   if(ecosystem=='EBS'){
     group.names <-c('Cross-shelf transport', 'Retention', 'Parental investment')
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
   
   # keep track of physical variable to plot
   phys.plot <- 0
   
   for(h in 1:grid.height){
     
     # empty plot to put names on
     plot(0, xlim=c(0,1), ylim=c(1, stocks + groups+2.5), xaxt='n', yaxt='n', bty='n', type='n')

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
   
     # for all the physical variables, plot the group level paramater distriubtion and the individual stock 
     # mean and 95% CI on that distribution
     for(w in 1:grid.width){
  	  if(phys.plot < phys){
  		  phys.plot <- phys.plot + 1
       
    		# Calculate the 95% CI for all the stocks and physical variables to get the x limits for plotting
    		median <- array(NA, stocks+groups+1)
    		lower <- array(NA, stocks+groups+1)
    		upper <- array(NA, stocks+groups+1)
  		 
    		# stock-level parameter
    		for(j in 1:stocks){
    		  median[j] <- median(beta[,j,phys.plot])
    		  lower[j] <- quantile(beta[,j,phys.plot], 0.025)
    		  upper[j] <- quantile(beta[,j,phys.plot], 0.975)
    		}
  		 
  		  # group-level parameter
  		  for(j in 1:groups){
  		    median[j+stocks] <- median(beta.mu[, phys.plot, j])
  		    lower[j+stocks] <- quantile(beta.mu[, phys.plot, j], 0.025)
  		    upper[j+stocks] <- quantile(beta.mu[, phys.plot, j], 0.975)
  		  }
  		 
  		  # ecosystem-level parameter
  		  median[j+stocks+1] <- median(beta.mu.mu.adj[, phys.plot])
  		  lower[j+stocks+1] <- quantile(beta.mu.mu.adj[, phys.plot], 0.025)
  		  upper[j+stocks+1] <- quantile(beta.mu.mu.adj[, phys.plot], 0.975)
  		  
  		  xlim <- max(c(abs(lower), upper))
       
  		  plot(0, xlim=c(-xlim, xlim), ylim=c(1, stocks + groups + 2.5), yaxt='n', bty='n', type='n', cex.axis=1)
  		  title <- paste('PC', phys.plot, '\n', PClabels[phys.plot], sep='')
  		  title(title, line=title.lines[phys.plot], font.main=1, cex.main=1.15, adj=0) # title with PC number
  		  
  		  
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
       lines(x=c(0, 0), y=c(0, stocks+groups+1.25), lty=2)
       
       # save the 95% CI
       results <- cbind(lower, median, upper)
       row.names(results) <- c(names, group.names, ecosystem)
       write.csv(results, paste(file.path, file.name, 'PC',phys.plot, '.csv', sep=''))
      }
    }
  }
  mtext('Parameter', 1, line=0, outer=T, cex=1)
  dev.off()
  graphics.off()
  
}
     