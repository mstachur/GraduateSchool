# BayesianHierarchicalModels.r
# For running Bayesian hierarchical model with multiple groups

# Megan Stachura
# Created June 19, 2012

##########################################################################
## set working directory
##########################################################################

wd<-'/Users/Megan/Dropbox/'  # on mac
usingMac <- TRUE

# wd <- 'C:/Users/Megan Stachura/Dropbox/' # on pc
# usingMac <- FALSE

setwd(wd)

#########################################################################
## inits
##########################################################################

library(coda)
library(runjags)
library(R2jags)
source('FATE recruitment/Recruitment code/PlotBUGSDiags.r')
source('FATE recruitment/Recruitment code/PlotGroupParams.r')
source('FATE recruitment/Recruitment code/PlotBUGSFit.r')
source('FATE recruitment/Recruitment code/PlotGroupParams2.r')
source('FATE recruitment/Recruitment code/CompareParamaterDistributions.r')
source('FATE recruitment/Recruitment code/ModelFitCorrelationByStock.r')
source('FATE recruitment/Recruitment code/CorrelationAutocorrelationAdjusted.r')
source('FATE recruitment/Recruitment code/AutocorrelatedDegreesOfFreedom.r')
source('FATE recruitment/Recruitment code/PlotBUGSFitScatterplot.r')
source('FATE recruitment/Recruitment code/PlotGroupParamsPaper.r')
source('FATE recruitment/Recruitment code/PlotGroupParamsPaperGrid.r')
source('FATE recruitment/Recruitment code/PlotGroupParamsAlreadyCalculated.r')

##########################################################################
## read in data
##########################################################################

# recruitment data
rec.all <- read.csv('FATE recruitment/Recruitment data/FATERecruitmentDeviations.csv', header=T, row.names=1) # recruitment data

# groupings
# grouping <- read.csv('FATE recruitment/Recruitment grouping hypotheses/Groupings_31Oct2012.csv', header=T, row.names=1)
grouping <- read.csv('FATE recruitment/Recruitment grouping hypotheses/Groupings_27June2013.csv', header=T, row.names=1)

# stock names
labels <- read.csv('FATE recruitment/Recruitment data/FATEresiduals_names.csv', header=T, stringsAsFactors=F)
stock.names <- labels$Name
stocks.names.full <- labels$NameFull

##########################################################################
# run JAGS
##########################################################################

# set which ecosystems you want to run JAGS for (can be multiple)
# all.ecosystems <- c('GOA','EBS','CC')

# create a list to hold all the results
jags.results.bsai <- vector("list", 30)
dic.results.bsai <- vector("list", 30)
cnt <- 1

hypothesis <- 1

ecosystem <- 'EBS'
if(ecosystem=='EBS'){
  physical.file.names <- c('BSAI_Freshwater runoff_PCA_scores', 'BSAI_Ice_PCA_scores', 'BSAI_SSH_PCA_scores', 'BSAI_SST_PCA_scores', 'BSAI_Wind_PCA_scores', 'BSAI_PCA_scores', 'BSAI_PCA_scores', 'BSAI_PCA_scores', 'BSAI_PCA_scores')
  physical.cols <- c(2,2,2,2,2,2,3,4,5)
} else if(ecosystem=='GOA'){
  physical.file.names <- c('GOA_FreshwaterRunoff_PCA_scores', 'GOA_SSH_PCA_scores_negative1', 'GOA_SST_PCA_scores', 'GOA_Upwelling_PCA_scores', 'GOA_PCA_scores', 'GOA_PCA_scores', 'GOA_PCA_scores', 'GOA_PCA_scores')
  physical.cols <- c(2,2,2,2,2,3,4,5)
#   physical.file.names <- c('GOA_SSH_PCA_scores', 'GOA_PCA_scores', 'GOA_PCA_scores', 'GOA_PCA_scores', 'GOA_PCA_scores')
#   physical.cols <- c(2,2,3,4,5)
}else if(ecosystem=='CC'){
  physical.file.names <- c('CC_Freshwater runoff_PCA_scores', 'CC_Sea level_PCA_scores', 'CC_SSH_PCA_scores', 'CC_SST_PCA_scores', 'CC_Upwelling_PCA_scores', 'CC_PCA_scores', 'CC_PCA_scores', 'CC_PCA_scores', 'CC_PCA_scores')
  physical.cols <- c(2,2,2,2,2,2,3,4,5)
#   physical.file.names <- c('CC_SSH_PCA_scores', 'CC_PCA_scores', 'CC_PCA_scores', 'CC_PCA_scores', 'CC_PCA_scores')
#   physical.cols <- c(2,2,3,4,5)
}

timer.start.total <- proc.time()
# for(physical.num in 1:length(physical.file.names)){
for(i in 1:length(physical.file.names)){
  
# get recruitment data and grouping data for the appropriate ecosystem
  if(ecosystem=='GOA'){
    goa.cols <- c(1:14)
#     goa.rows <- c(19:69) # start in the first year recruitment data is available
    goa.rows <- c(19:69) # start in the first year recruitment data is available, end in the last year when ROMS SSH data is available
    rec <- rec.all[goa.rows, goa.cols] # get recruitment data only for appropriate stocks and years
    rec.original <- rec # JAGS changes the rec matrix so keep an original for plotting
    group.index <- grouping[goa.cols, hypothesis] # get group indices
    allPhysical <- read.csv(paste('FATE recruitment/Recruitment data/Physical data/GOA/', physical.file.names[i], '.csv', sep=''), header=T, row.names=1)
    allPhysical <- allPhysical[,1:physical.cols[i]]
    names <- stock.names[goa.cols] # names of stocks in this ecosystem
    order <- c(1,2,11,6,12,7,13,4,14,8,3,5,10,9)
  }else if(ecosystem=='EBS'){
    ebs.cols <- c(15:28)
    ebs.rows <- c(14:69) # 1953-2008
    rec <- rec.all[ebs.rows, ebs.cols]
    rec.original <- rec
    group.index <- grouping[ebs.cols, hypothesis]
    allPhysical <- read.csv(paste('FATE recruitment/Recruitment data/Physical data/BSAI/', physical.file.names[i], '.csv', sep=''), header=T, row.names=1)
    allPhysical <- allPhysical[,1:physical.cols[i]]
    names <- stock.names[ebs.cols]
#     order <- c(1,2,11,5,12,6,7,13,14,9,10,4,8,3) 
    order <- c(3,1,11,5,12,6,7,13,14,9,10,4,8,2)
  }else if(ecosystem=='CC'){
#     cc.cols <- c(29:52)
    cc.cols <- c(29:51) # without Pacific mackerel
    cc.rows <- c(29:69) # limit to years that recruitment and physical data are available
    rec <- rec.all[cc.rows, cc.cols]
    rec.original <- rec
    group.index <- grouping[cc.cols, hypothesis]
    allPhysical <- read.csv(paste('FATE recruitment/Recruitment data/Physical data/CC/', physical.file.names[i], '.csv', sep=''), header=T, row.names=1)
    allPhysical <- allPhysical[,1:physical.cols[i]]
    names <- stock.names[cc.cols]
#     order <- c(15,2,7,8,9,11,17,18,16,21,22,6,5,3,4,1,13,14,19,12,23,20,10,24)
    order <- c(15,2,7,8,9,11,17,18,16,21,22,6,5,3,4,1,13,14,19,12,23,20,10) # without Pacific mackerel
  }

  physical.names <- colnames(allPhysical) # names of physical variables, for plotting
  rec <- as.matrix(rec) # turn rec into a matrix, jags doesn't like data frames
  allPhysical <- as.matrix(allPhysical) # turn allPhysical into a matrix, jags doesn't like data frames
  stocks <- ncol(rec) # number of stocks
  years <- nrow(rec) # number of years
  phys <- ncol(allPhysical) # number of physical variables
  groups <- length(levels(as.factor(group.index))) # number of groups
  allPhysical <- scale(allPhysical, center=T, scale=T) # standardize physical data to mean=0, sd=1
  
  #set up the JAGS data
  rec.data <- list('rec', 'stocks', 'years', 'allPhysical', 'phys', 'groups', 'group.index')
  
  # set up the JAGS initial paramater values
  # may have to update this with actual numbers
  rec.inits <- function(){
    list("a"=runif(stocks, min=-0.5, max=0.5), # mean recruitment level for each stock
         "rec.sigma"=runif(stocks, min=0, max=1), # sd for each stock
         "eta.beta"=array(rnorm(stocks*phys,0,1), c(stocks, phys)), # residual of the stock level beta the group level beta
         "mu.eta.beta.group"=array(rnorm(phys,0,1), phys), # mean residual of group level betas from the grand mean beta
         "beta.mu.mu"=array(rnorm(phys,0,1), phys), # grand mean across all group level betas
         "sigma.beta.group"=array(runif(phys, min=0, max=1), phys), # standard deviation across all group level betas
         "eta.beta.group"=array(rnorm(phys*groups,0, 1), c(phys, groups)), # residual of beta for each group from the grand mean beta
         "tau.i"=array(runif(phys*groups, 1,10), c(phys, groups)), # inverse variance (precision) of residuals of stock level betas from the group level betas
         "xi"=array(rnorm(phys*groups,0.1,0.5), c(phys, groups))) # for scaling the eta.beta
  }
  
  # paramaters to save
   #rec.parameters <- c('beta', 'beta.mu.mu.adj', 'eta.beta.group.adj', 'sigma.beta.group', 'beta.sd',
                        #'a', 'rec.sigma', 'beta.mu', 'rec.save')
                  
  rec.parameters <- c('beta.mu.mu.adj', 'eta.beta.group.adj', 'eta.beta.adj', 'sigma.beta.group', 'beta.mu',
                      'tau.i', 'beta.sd', 'beta', 'a', 'rec.sigma', 'rec.save', 'xi')

  file.name <- paste('FATE recruitment/Recruitment code/model_phys_cauchy_residuals_new', phys, '.txt', sep='')
  mcmc.chainLength <- as.integer(200000)  # burn-in plus post-burn
  mcmc.burn <- as.integer(100000)
  mcmc.thin = 200
#   print(chain.length <- (mcmc.chainLength-mcmc.burn)/mcmc.thin)
  mcmc.chains = 3       # needs to be at least 2 for DIC
  dic.samples = (mcmc.chainLength-mcmc.burn)
  
  # start the timer
  timer.start <- proc.time()
  
  # run model
  jags.results.bsai[[cnt]] = jags(model.file=file.name, rec.data, inits = rec.inits, parameters.to.save= rec.parameters, n.chains = mcmc.chains, n.burnin = mcmc.burn, n.thin = mcmc.thin, n.iter = mcmc.chainLength, DIC = FALSE)
  
  #calculate DIC
  dic.results.bsai[[cnt]] <- dic.samples(model=jags.results.bsai[[cnt]]$model, n.iter=dic.samples, thin=mcmc.thin, type='pD')
  
#   print(paste(physical.file.names[i], physical.cols[i]))
#   print(dic.results.cc[[cnt]])
  
  # print the run time for this model only
  print(run.time.in.min <- (proc.time()-timer.start)[3]/60)
  
  cnt <- cnt+1
}

# print the total run time
print(run.time.in.min <- (proc.time()-timer.start.total)[3]/60)
  

################################################################################
# Look at the model results
################################################################################

# attach model results
attach.jags(jags.results.goa[[cnt]])

file.path <- 'FATE recruitment/Model fit results/BSAI/New groups sigma.beta~dunif(0,10)/All5/'

# # to reverse GOA SSH PC1
# beta[,,1] <- -beta[,,1]
# beta.mu[,1,] <- -beta.mu[,1,]
# beta.mu.mu.adj[,1] <- -beta.mu.mu.adj[,1]

# get estimated paramaters as MCMC object
allOutsMCMC <- NULL
for(x in 1:dim(beta)[3]) # beta for all physical variables
  allOutsMCMC <- cbind(allOutsMCMC, beta[,,x])
for(x in 1:dim(eta.beta.group.adj)[3]) # eta.beta.group.adj for all physical variables
  allOutsMCMC <- cbind(allOutsMCMC, eta.beta.group.adj[,,x])
for(x in 1:dim(beta.sd)[3]) # beta.sd for all groups
  allOutsMCMC <- cbind(allOutsMCMC, beta.sd[,,x])
# add in everything else that is 2D
allOutsMCMC <- cbind(allOutsMCMC, beta.mu.mu.adj, sigma.beta.group, a, rec.sigma)
# make it an MCMC object
allOutsMCMC <- mcmc(allOutsMCMC)

#open the plotting window
if(usingMac==TRUE) quartz(w=10, h=7) else windows(w=10, h=7)
par(ask=T) # ask before doing the next plot

# par(mfrow=c(5,5), mar=c(2,2,1,1))
# plot(allOutsMCMC)
# densplot(allOutsMCMC)

# plot the jags output
plot(jags.results.goa[[cnt]])

# look at diagnostics
PlotBUGSDiags(allOutsMCMC)

# look at correlation in more depth
# for each physical variable, betas are positively correlated with each other
# eta.beta.group.adj negatively correlated with betas for other groups
# betas postively correlated with beta.mu.mu.adj
# cor <- round(data.frame(crosscorr(allOutsMCMC)), digits=2)
# write.csv(cor, 'FATE/Model fit results/GOA_correlations_31Oct2012.csv')

# plot the parameter distributions
if(usingMac==TRUE) quartz(w=10, h=7) else windows(w=10, h=7)
pdf(paste(file.path, 'Parameter distributions old.pdf', sep=''))
for(x in 1:groups){
  PlotGroupParams(group.index=group.index, group.num=x, phys=phys,
                  stock.names=names, plot.rows=2, plot.cols=2, usingMac=TRUE)
}
dev.off()
graphics.off()

# plot paramaters the new way
PlotGroupParams2(stocks, groups, names, phys, order, ecosystem, file.path=file.path, file.name='Parameter distributions', usingMac=usingMac)

# save model fits
ModelFitCorrelationByStock(stocks, years, names, rec.original, file.path, return.result=F)

# plot the model fits
PlotBUGSFit(rec.original, rec.save, names, stocks, years, order, plot.rows=5, plot.cols=3, file.path=file.path, file.name='Model fit', ecosystem=ecosystem, usingMac=usingMac)

# plot a scatterplot of the model fits
PlotBUGSFitScatterplot(rec.original, rec.save, names, stocks, years, order, plot.rows=5, plot.cols=3, file.path=file.path, file.name='Model fit scatterplot', ecosystem=ecosystem, usingMac=usingMac)

# for CC second page
# PlotBUGSFit(rec.original, rec.save, names, stocks, years, order, plot.rows=5, plot.cols=3, plot.height=9, file.path=file.path, file.name='Model fit', ecosystem=ecosystem, usingMac=usingMac)

CompareParamaterDistributions(groups, group.index, phys, physical.names, names, stocks, file.path=file.path, file.name='Compare groups',  usingMac=TRUE, x.min=-.5, x.max=.5)

##########################################################################################
# plot the model fits
# PlotBUGSFitGrouped(rec=rec.original, rec.save=rec.save, stock.names=names, groups=groups,
#                    group.index=group.index, usingMac=usingMac)

# PlotGroupParamsForPoster(stocks, groups, names, phys, order, ecosystem, file.path=file.path, file.name='Parameter distributions poster', usingMac=usingMac)

# PlotGroupParamsOnlyGroupLevel(stocks, groups, names, phys, order, ecosystem, file.path=file.path, file.name='Parameter distributions group level only', usingMac=usingMac)


##########################################################################################
# plot parameters with the values aleady calculated and saved
#BSAI
PClabels <- c('Low ice cover index (t-1 and t) \n and high spring SST (t-1 and t)', 'Positive SSH anomalies \n throughout the Bering Sea', 'Increased northeasterly \n winter cross-shelf wind (t)', 'Increased southwesterly winter \n cross-shelf wind (t-1)', 'Increased southeasterly winter \n along-shelf wind (t-1)')
PlotGroupParamsAlreadyCalculated(stocks, groups, names, phys, order, ecosystem, file.path='FATE/Model fit results/BSAI/New groups sigma.beta~dunif(0,10)/All5/', file.name='Parameter Disbriubtion PCs labeled', width=(3+phys*2), height=(1.5+stocks/5), usingMac=T)
PlotGroupParamsPaper(stocks, groups, names, phys, order, ecosystem, file.path='FATE/Model fit results/BSAI/New groups sigma.beta~dunif(0,10)/All5/', file.name='Parameter Disbriubtion PCs labeled with legend', width=(3+phys*2), height=(1.5+stocks/5), usingMac=T)

# in grid for BSAI
PlotGroupParamsAlreadyCalculatedGrid(stocks, groups, names, phys, order, ecosystem, file.path='FATE recruitment/Model fit results/BSAI/New groups sigma.beta~dunif(0,10)/All5/', file.name='Parameter Disbriubtion PCs labeled Grid', grid.width=3, grid.height=2, usingMac=T)
PlotGroupParamsPaperGrid(stocks, groups, names, phys, order, ecosystem, file.path='FATE recruitment/Model fit results/BSAI/New groups sigma.beta~dunif(0,10)/All5/', file.name='Parameter Disbriubtion PCs labeled Grid', grid.width=3, grid.height=2, usingMac=T)

# GOA
PClabels <- c('Positive coastal SSH \n anomalies', 'Negative offshore SSH \n anomalies')
PlotGroupParamsAlreadyCalculated(stocks, groups, names, phys, order, ecosystem, file.path='FATE recruitment/Model fit results/GOA/sigma.beta~dunif(0,10)/SSH/', file.name='Parameter Disbriubtion PCs labeled', width=(3+phys*2), height=(1.5+stocks/5), usingMac=T)

# CC
PClabels <- c('High sea level (t-1 and t)', 'High sea level (t-1) and low \n sea level (t)')
title.lines <- c(-1,-2.2)
PlotGroupParamsAlreadyCalculated(stocks, groups, names, phys, order, ecosystem, file.path='FATE/Model fit results/CC/Sea level/', file.name='Parameter Disbriubtion PCs labeled', width=(3+phys*2), height=(1.5+stocks/5), usingMac=T, title.lines)
PlotGroupParamsPaper(stocks, groups, names, phys, order, ecosystem, file.path='FATE/Model fit results/CC/sigma.beta~dunif(0,10)/Sea level/', file.name='Parameter Disbriubtion PCs labeled', width=(3+phys*2), height=(1.5+stocks/5), usingMac=T, title.lines)

##################################################################################################
# Plot GOA model fits for presentation

rec.predicted <- read.csv('FATE recruitment/Model fit results/GOA/sigma.beta~dunif(0,10)/SSH/Median predicted recruitment.csv', header=T, row.names=1)

PlotBUGSFit_Presentation(rec.original, rec.predicted, names, order, plot.rows=4, plot.cols=4, plot.height=10, file.path='FATE recruitment/Model fit results/GOA/sigma.beta~dunif(0,10)/SSH/Model fit for presentation' usingMac=TRUE)



##################################################################################################
