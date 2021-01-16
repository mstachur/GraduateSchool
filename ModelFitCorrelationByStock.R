# ModelFitCorrelationByStock
# Calculate the correlation between the median model predicted value and the observed recruitment by stock
# to see how well the model does for the stocks individually

ModelFitCorrelationByStock <- function(stocks, years, names, rec, file.path, return.result=FALSE){
  
  # calculate the median predicted values
  predicted.median <- array(NA, c(years,stocks))
  predicted.median <- data.frame(predicted.median)
  row.names(predicted.median) <- row.names(rec)
  names(predicted.median) <- names(rec)
  for(i in 1:stocks){
    for(j in 1:years){
      predicted <- rec.save[,j,i]
      predicted.median[j,i] <- median(predicted)
    }
  }
  
  # created a matrix to hold the correlation results
  results <- array(NA, dim=c(stocks, 6))
  results <- data.frame(results)
  row.names(results) <- names
  names(results) <- c('Cor', 'CorAppended', 'Sig', 'AutoSig', 'N', 'AutoN')
  
  # calculation the correlation of the pearson correlation coefficient
  for(i in 1:stocks){
      
    # significance not accounting for autocorrelation
    results$Sig[i] <- cor.test(x=rec[,i], y=predicted.median[,i], alternative='greater',
                        method='pearson', use='na.or.complete')$p.value
    
    # calculate the significance with autocorrelation
    sig.auto <- CorrelationAutocorrelationAdjusted(x=rec[,i], y=predicted.median[,i])
    results$Cor[i] <- sig.auto[1]
    results$CorAppended[i] <- sig.auto[1]
    results$AutoSig[i] <- sig.auto[2]
    results$N[i] <- sig.auto[3]
    results$AutoN[i] <- sig.auto[4]
    
    # if it's significant, add an asterick after the correlation coefficient to denote this
    if(results$AutoSig[i] < 0.001){ 
      results$CorAppended[i] <- paste(results$Cor[i], '***', sep='')
    }else if(results$AutoSig[i] < 0.01){ 
      results$CorAppended[i] <- paste(results$Cor[i], '**', sep='')
    }else if(results$AutoSig[i] < 0.05){ 
        results$CorAppended[i] <- paste(results$Cor[i], '*', sep='')
    }else if(results$AutoSig[i] < 0.1){ 
        results$CorAppended[i] <- paste(results$Cor[i], '.', sep='')
    }
  }
  
  if(return.result==F){
    # save the correlation and significance files
    write.csv(results, paste(file.path, 'Model correlations with observed.csv', sep=''))
    
    # save the median predicted values
    write.csv(predicted.median, paste(file.path, 'Median predicted recruitment.csv', sep=''))
  }else return(results)
}