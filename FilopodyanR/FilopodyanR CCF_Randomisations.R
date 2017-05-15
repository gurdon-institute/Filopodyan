# TIP FLUORESCENCE & MOVEMENT - subcluster analysis, randomisations

# This script is part of a suite of scripts for analysis of filopodia dynamics 
# using the Fiji plugin Filopodyan. 
#
# Data input: requires an .Rdata file from upstream script ('Filpodyan CCF.R') 
# Data output: a large number of randomisations of the original dataset, and a plot of 
# their collective CCFs

rm(list = ls())

# ---------------------------------------------------------------------------
# Required packages:

# install.packages('purrr', dependencies= TRUE, repos='http://cran.rstudio.com/')
# install.packages("RColorBrewer", dependencies=TRUE, repos="http://cran.rstudio.com/")
# install.packages("gplots", dependencies=TRUE, repos="http://cran.rstudio.com/")
library(purrr)
library(RColorBrewer)
library(gplots)

# Requirements loaded from parent script:

# - Data from the parent script ('BounderR_CCFs.R'):
#     dataframe 'all.move' (movement over time)
#     dataframe 'tip.f' (fluorescence over time)
#     dataframe 'ccf.tip.dctm' (cross-correlation table from the two dataframes above)

# - Functions from parent script ('BounderR_CCFs.R'):
#     'Count', CI', 'ExtractBlockIndex', 'BlockReshuffle', 'DrawErrorAsPolygon'

# Example datasets:

# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toGC/LastWorkspace_CCFs.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC/LastWorkspace_CCFs.Rdata')

stopifnot(exists('all.move'))
stopifnot(exists('tip.f'))
stopifnot(exists('ccf.tip.dctm'))


#---------------------------------------------------------------------------
# 1. Manually select size of top subcluster based on the heatmap:

dev.new()
myHeatmap(ccf.tip.dctm)

guide.size <- 9  # size of top subcluster in the initial CCF analysis (input manually) 

target.range <- c(0.85, 1.15) * guide.size  # <-- Tolerance range for clusters of randomised data
target.min <- ceiling(target.range[1])
target.max <- floor(target.range[2])
target.size <- (target.min:target.max)
target.size

# For my datasets, guide.size: 
#  ENA: 14
#  VASP: (9?)  # very high if 9; slightly lower at 37
#  TOCA: 9


#---------------------------------------------------------------------------
# 2. FUNCTIONS FOR CLUSTERING OPERATIONS:

#----------
# Original clustering functions (as in parent script):

GoCluster <- function(x, n.clusters) {
  map.input <- t(x)
  distance <- dist(map.input[, 18:24], method = "euclidean")
  cluster <- hclust(distance, method = "complete")
  cutree(cluster, k = n.clusters)
}

myHeatmap <- function(x) {
  map.input = t(x)
  distance <- dist(map.input[, 18:24], method = "euclidean")
  cluster <- hclust(distance, method = "complete")
  heatmap(map.input, Rowv = as.dendrogram(cluster), Colv = NA, xlab = "Lag", col = brewer.pal(12, "YlGnBu"), 	scale = "none")	
}

	
# example use: 
# myHeatmap(ccf.tip.dctm)

nthSubcluster <- function(x, n.clusters, nth) {
  which(GoCluster(x, n.clusters = n.clusters) == nth)
}
nthSubclusterOthers <- function(x, n.clusters, nth) {
  which(GoCluster(x, n.clusters = n.clusters) != nth)
}

#----------
# FUNCTIONS FOR ITERATIVE SEARCH THROUGH NUMBERS OF CLUSTERS (until Top Cluster ~ Guide Size):

# Requirements: identify "top cluster" 
# loop k of cluster numbers until top cluster == guide size

ListSubclusters <- function(x, n.clusters = 2) {
  
  # Takes data frame x, outputs list of subtables as per specified number of clusters
  
  # Create a list of all subclusters:
    for (i in 1:n.clusters) {
      if (i == 1) {all.subcl = list()}
      cluster.i.index <- which(GoCluster(x, n.clusters) == i)
      all.subcl[[i]] <- x[, cluster.i.index]
    }
  return(all.subcl)
}


whichMax <-  function(x) which(x == max(x))

SizeMaxSubcluster <- function(x, n.clusters = 2) {
  
  # Takes data frame x, returns the size of the subcluster with highest mean at lag = 0 (at specified n clusters)
  #   - Calculate means at lag = 0
  #   - Find subcluster with highest mean at 0
  #   - Return the size of this top subcluster
  
  all.subcl <- ListSubclusters(x, n.clusters)
  clust.means <- lapply(all.subcl, function(x) apply(x["0", ], 1, mean, na.rm = TRUE)) # This trips single-element subclusters (number of dimensions)
  top.cluster <- whichMax(unlist(clust.means))
  top.size <- ncol(all.subcl[[top.cluster]])
  return(top.size)  
}

TopSubcluster <- function(x, n.clusters = 2) {
  
  # Takes data frame x, returns top most correlated cluster
  
  all.subcl <- ListSubclusters(x, n.clusters)
  clust.means <- lapply(all.subcl, function(x) apply(x["0", ], 1, mean, na.rm = TRUE)) # This trips single-element subclusters (number of dimensions)
  top.cluster <- whichMax(unlist(clust.means))
  output <- list(
    "top.cluster.ID" = top.cluster,
    "top.cluster"   = all.subcl[[top.cluster]]
  )
  return(output)
}

# e.g.
# ListSubclusters(ccf.tip.dctm, 7)[[7]]
# TopSubcluster(ccf.tip.dctm, 5)
# myHeatmap(ccf.tip.dctm)
# TopSubcluster(ccf.tip.dctm, 2)

# To get target.size from original ### ADDED 10.3.

# n.clust.in.orig <- 2
# guide.size <- ncol(as.data.frame(TopSubcluster(ccf.tip.dctm, n.clust.in.orig)["top.cluster"]))
# target.size <- c(-1, 0, 1) + guide.size  # Replaced by stuff on top of script!

SearchClusterSpace <- function(x, target.size = target.size) {
  top.clust.size = NA
  initial.n.clusters = 1
  curr.n.clusters = initial.n.clusters
  while ((top.clust.size %in% target.size) == FALSE) {
    curr.n.clusters = curr.n.clusters + 1
    top.clust.size <- SizeMaxSubcluster(x, curr.n.clusters)
  }
  top.cluster.IDs = nthSubcluster(x, curr.n.clusters, TopSubcluster(x, n.clusters = curr.n.clusters)$top.cluster.ID)
  other.IDs = nthSubclusterOthers(x, curr.n.clusters, TopSubcluster(x, n.clusters = curr.n.clusters)$top.cluster.ID)
  
  output <- list(
    "n.clusters" = curr.n.clusters,
    "top.cluster.ID" = TopSubcluster(x, curr.n.clusters)[["top.cluster.ID"]],
    "top.clust.size" = top.clust.size,
    "top.cluster" = TopSubcluster(x, curr.n.clusters)[["top.cluster"]],
    "top.cluster.IDs" = top.cluster.IDs,
    "other.IDs" = other.IDs
    )
  return(output)
}

SearchClusterSpace(ccf.tip.dctm, target.size = target.size)

#------------------------------------------------------------------------------
#   BOOTSTRAPPING RANDOMISATIONS OF TIP MOVEMENT  
#------------------------------------------------------------------------------

### i.e. create 2500 randomised datasets not 1.
### (added to code on 8.3.2017) 


n.bootstrap = 10000  # <- Make this larger than the number of required clusters, not every 
                    #    dataset clusters well (allow 75% failure rate in the first instance)

#------------------------------------------------------------------------------
### 1. GENERATE RANDOMISATIONS:

# Create an empty randomisation table for each repeat (bootstrap) of randomisation,
# populate the table with results of BlockReshuffle on original data (filo by filo),
# and finally pass the resulting table for each boostrap to the list of randomisation tables.


CreateBootstrapRandomisations <- function(x, n.bootstrap) {
  
  for (boot in 1:n.bootstrap) {
    
    # Quality control (n.bootstrap 0 would cause failure):
    stopifnot(n.bootstrap >= 1)
    
    # In first run of the loop, create a list to store all randomisations 
    if (boot == 1) {list.all.rand <- list()}
    
    # Make new randomisation with each iteration of the loop:
    curr.randomisation <- matrix(NA, ncol = ncol(x), nrow = nrow(x))
    colnames(curr.randomisation) = colnames(x)
    for (i in seq_along(colnames(x))) {
      curr.randomisation[, i] <- BlockReshuffle(x[, i])
    }  
    
    # Add the new randomisation to the list of all: 
    list.all.rand[[boot]] <- curr.randomisation
    
    # Monitor loop progress:
    if (boot %% 25 == 0) {
      print(paste0(floor (boot/n.bootstrap * 100), "% completed"))
    }
   
  }
  stopifnot(length(list.all.rand) == n.bootstrap)
  return(list.all.rand)
}

list.all.move.rand <- CreateBootstrapRandomisations(all.move, n.bootstrap)

#------------------------------------------------------------------------------
### 2. CALCULATE CCF TABLES FOR ALL RANDOMISED DATASETS:

### Added to code ON 8.3.2017

list.ccf.randomised <- list()

for (boot in 1:n.bootstrap) {  
  curr.rand.move = list.all.move.rand[[boot]]
  
  # Create new table for CCFs of randomised dataset (using current randomisation only) 
  ccf.curr.randomisation = data.frame(matrix(NA, ncol = ncol(all.move), nrow = 2*maxlag + 1))
  colnames(ccf.curr.randomisation) = colnames(all.move)
  rownames(ccf.curr.randomisation) = lag.in.s
  
  n.filo = ncol(all.move)
  
  for (i in 1:n.filo) {
    
    # Create CCF for each filo, pass all into one table
    ccf.i <- ccf(tip.f[, i], curr.rand.move[, i], lag.max = 20, na.action = na.pass, plot = FALSE) 
    ccf.curr.randomisation[, i] <- ccf.i
    
    # Pass all CCFs for the current randomisation into a new list item
    list.ccf.randomised[[boot]] <- ccf.curr.randomisation
    rm(ccf.i)
  }
  rm(curr.randomisation)
 
  # Monitor loop progress:
  if (boot %% 25 == 0) {
    print(paste0(floor (boot/n.bootstrap * 100), "% completed"))
  } 
}


#------------------------------------------------------------------------------
### 3. CLUSTER ALL CCF TABLES  

# Run as many CCF tables through clustering as required to reach specified number of 
# successfully clustered randomised datasets

# 'new.list' stores result for all 2500 randomisations with NULL unsuccesful runs recorded; 
# 'boot.top.cluster.CCFs' stores results after cleaning up unsuccesful runs

n.bootstrap.clusters = 1000  # <- HOW MANY CLUSTERS OF RANDOMISED DATASETS ARE REQUIRED?


#   This is the crux of the section! The loop underneath does the following: 
#   1. Creates an empty list to store new datasets on first iteration
#   2. Keeps count of 'successful' runs of the loop. A successful run means
#      that a randomised dataset could be clustered in such a way that the max
#      correlating cluster (top correlating subcluster, "TCS") had the required 
#      number of elements (as the original dataset, +- 1) (this number is specified 
#      in the SearchClusterSpace function as the 'target.size' argument).
#   3. While the count is less than specified (e.g. < 500 clusters), it keeps taking
#      new randomisations from the large pool of randomisations (list.ccf.randomised, 
#      e.g. 2500 randomisations), checks whether they can be 'successfully' clustered
#      - if yes, adds the result (of SearchClusterSpace) to the new list
#      - if no, adds a NULL list entry instead and moves to the next randomisation in
#      the pool.


for (i in 1:n.bootstrap) {
  if (i == 1) {new.list = list()}
  
  # Number of fruitful bootstrap clustering runs:
  n.count <- sum(unlist(lapply(new.list, function(x) !is.null(x))))
 
  # While count number is less than specified, keep running this loop:
  if(n.count < n.bootstrap.clusters) {
    try(SearchClusterSpace(list.ccf.randomised[[i]], target.size = target.size), silent = TRUE)
    try(new.list[[i]] <- SearchClusterSpace(list.ccf.randomised[[i]], target.size = target.size), silent = TRUE)
    n.count <- sum(unlist(lapply(new.list, function(x) !is.null(x))))	
   # print(paste("n.count = ", n.count))
   # break
  } else {stop}
  # Monitor loop progress:
  if (n.count %% 10 == 0) {
    print(paste0(floor (n.count/n.bootstrap.clusters * 100), "% completed"))
  }
}


#------------------------ 
### A bit of cleaning up:

#new.list[[lapply(new.list, function (x) !is.null(x))]]

# non-null elements of list: 
notnull <- lapply(new.list, function (x) !is.null(x)) # (boolean vector)
which(notnull == TRUE) # (numeric vector)

# List of lists needs transposing
# Requirement: purrr package, function transpose()

new.list.non.null <- new.list[which(notnull == TRUE)]
new.list.non.null[1:10]
new.list.transposed <- transpose(new.list.non.null) # Transpose from PURRR package

boot.top.cluster.CCFs <- new.list.transposed[["top.cluster"]] # This works!

#------------------------- 
# Extract mean CCFs of the top correlating subcluster for each randomisation:

list.CCF.means <- lapply(boot.top.cluster.CCFs, function(x) apply(x, 1, mean, na.rm = TRUE))
list.CCF.ci <- lapply(boot.top.cluster.CCFs, function(x) apply(x, 1, CI))
df.CCF.means <- as.data.frame(list.CCF.means); colnames(df.CCF.means) <- as.character(1:ncol(df.CCF.means))
#df.CCF.CIs <- as.data.frame(list.CCF.ci); colnames(df.CCF.means) <- as.character(1:ncol(df.CCF.means)))

# Mean and CI of CCFs of all bootstrap repeats:
mean.CCF.boot <- apply(df.CCF.means, 1, mean, na.rm = TRUE) 
CI.CCF.boot <- apply(df.CCF.means, 1, CI) 


# Implement weighting according to length of time series?
# MeanByFiloWeight <- function(x) weighted.mean(x, w = weights.vec)
# list.CCF.w.means <- lapply(boot.top.cluster.CCFs, function(x) apply(x, 1, MeanByFiloWeight)) 
# Complicated at this point, weights for each subcluster will be different. Would need to extract 
# this upstream when creating the lists in the first place! 



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# SAVE THE DATA:

setwd(metalist[[1]]$Loc)
getwd()
n.bootstrap.clusters
name = paste0("LastWorkspace_", n.bootstrap.clusters, "_randomisations.Rdata")
save.image(name)

#------------------------------------------------------------------------------
# LOAD DATA FOR PLOTTING:

rm(list = ls())

 load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01/LastWorkspace_1000_randomisations.Rdata')

# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01/LastWorkspace_1000_randomisations.Rdata')
 
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_TOCA/Huang4-01/LastWorkspace_1000_randomisations.Rdata')

setwd(metalist[[1]]$Loc); getwd()
# setwd("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC")

#-----------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

curr.pal = brewer.pal(11, "RdBu")
curr.cols = curr.pal[c(4, 10, 2)] # "#41B6C4" "#225EA8" "#EDF8B1" for "YlGnBu"
curr.cols.30 = paste0(curr.cols, "30")
curr.cols.60 = paste0(curr.cols, "60")
curr.cols    # "#F4A582" "#2166AC" for "RdBu"


#------------------------------------------------------------------------------
# 4. PLOT ALL RANDOMISATIONS:

# 4a) Line plot of top subcluster CCF (v offset) for all randomisations, 
#     compared with real dataset


# 4a(i) Figure out the top correlating subcluster from actual data:

SearchClusterSpace(ccf.tip.dctm, target.size = target.size)$top.cluster.IDs

 # real.top.cluster <- TopSubcluster(ccf.tip.dctm, 5)["top.cluster"]
  real.top.cluster <- ccf.tip.dctm[, SearchClusterSpace(ccf.tip.dctm, target.size = target.size)$top.cluster.IDs]
  real.top.cluster <- as.data.frame(real.top.cluster)
  mean.ccf.real.top.cluster <- apply(real.top.cluster, 1, mean, na.rm = TRUE)


# 4a(ii): Line plot of all randomised TCS CCFs: 

dev.new(width = 5, height = 4)
par(mfrow = c(1,1))
par(mar = c(5,4,2,1) + 0.1)
plot(NULL, 
     xlim = range(lag.in.s),
     ylim = 1.1*range(list.CCF.means, mean.ccf.real.top.cluster),
     xlab = "Offset [s]",
     ylab = "CCF")

stopifnot(length(list.CCF.means) > 0)
for (i in 1:length(list.CCF.means)) {
  #print(i)
  lines(lag.in.s, list.CCF.means[[i]],
        #col = "grey")
        col = "#AAAAAA20")
}

  lines(lag.in.s, mean.ccf.real.top.cluster,
      col = "#225EA8",
      lwd = 3)
  legend("topright", legend = "Observed", text.col = curr.cols[2], bty = "n")
  legend("topleft", legend = "Randomised", text.col = "#AAAAAA", bty = "n")
 
  abline(h = 0); abline(v = 0, lty = 3)

	setwd(metalist[[1]]$Loc); getwd()
	dev.copy(pdf, "Rplot_Randomisations_CCF_Lines.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()


  # ALTERNATIVE OPTION: use CI (instead of many lines)
  # ci1 = mean.CCF.boot + CI.CCF.boot
  # ci2 = mean.CCF.boot - CI.CCF.boot
  # DrawErrorAsPolygon(lag.in.s, ci1, ci2, tt = 0:41, col = "white", border = "magenta")
  # lines(lag.in.s, mean.CCF.boot, lwd = 1, col = "black")  


data.frame("lag" = seq(-40,40,2), "mean" = mean.CCF.boot, "CI" = CI.CCF.boot)

getwd()


# 4a(iii): choose a representative example from all bootstrap CCFs

# The mean at offset 0 is:

means.at.lag.0 <- as.data.frame(list.CCF.means)[21, ]
colnames(means.at.lag.0) <- as.character(1:500)
 
mean.at.0 <- mean(unlist(means.at.lag.0), na.rm = TRUE)
distance.from.mean <- abs(means.at.lag.0 - mean.at.0)
representative <- which(distance.from.mean == min(distance.from.mean))

dev.new(width = 5, height = 4)
par(mfrow = c(1,1))
par(mar = c(5,4,2,1) + 0.1)
plot(NULL, 
     xlim = range(lag.in.s),
     ylim = 1.1*range(list.CCF.means, mean.ccf.real.top.cluster),
     xlab = "Offset [s]",
     ylab = "CCF")

stopifnot(length(list.CCF.means) > 0)
for (i in 1:length(list.CCF.means)) {
  #print(i)
  lines(lag.in.s, list.CCF.means[[i]],
        #col = "grey")
        col = "#AAAAAA20")
}

  lines(lag.in.s, mean.ccf.real.top.cluster,
      col = "#225EA8",
      lwd = 3)
  legend("topright", legend = "Observed", text.col = curr.cols[2], bty = "n")
  legend("topleft", legend = "Randomised", text.col = "#AAAAAA", bty = "n")
 
  abline(h = 0); abline(v = 0, lty = 3)
lines(lag.in.s, list.CCF.means[[representative]],
	col = "black", lty = 3)
			
	setwd(metalist[[1]]$Loc); getwd()
	dev.copy(pdf, "Rplot_Randomisations_CCF_Lines_withRep.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()
	
# matching it up with its heatmap is not a trivial plotting task, see next section!


# 4b) Plot the distribution of simulations as ***histogram*** at offset = 0


hilo = range(means.at.lag.0, mean.ccf.real.top.cluster["0"], na.rm = TRUE)
 
dev.new(width = 5, height = 3.5)
	par(mar = c(5,4,1,1)+0.1)
	hist(unlist(means.at.lag.0), breaks = 25, plot = TRUE,
	     col = "grey", border = "white",
	     xlim = c(
	     	min(0.8 * hilo),	     
	      	max(1.1* hilo)),
	     main = "",
	     xlab = "Mean CCF"
	     )
	abline(v = mean.ccf.real.top.cluster["0"],
	       col = curr.cols[2], lty = 2)
	 legend("topright", legend = "Observed", text.col = curr.cols[2], bty = "n")
	 legend("topleft", legend = "Randomised", text.col = "grey", bty = "n")
	 box()

	setwd(metalist[[1]]$Loc); getwd()
	dev.copy(pdf, "Rplot_Randomisations_CCF_Histogram.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()
	

# 4c) Plot the distribution of simulations as ***CDF*** at offset = 0

CdfPlot <- function(x, ...) {
	plot(ecdf(x),
		main = "",
		ylab = "Cumulative distribution",
		xlab = "CCF at offset = 0",
		cex = 0,
		lwd = 3,
		verticals = TRUE,
		...)
	# vertical line:
	x.median = median(unlist(x), na.rm = TRUE)
	arrows(x.median, 1, x.median, 0.0, 
		length = 0.0, code = 3, angle = 180, col = "grey", lty = 1)
}

dev.new(width = 5, height = 3.5)
	par(mar = c(5,4,1,1)+0.1)
	CdfPlot(means.at.lag.0, 	
		xlim = c(
	     	min(0.8 * hilo),	     
	      	max(1.1* hilo)),
		col = "grey")

arrows(mean.ccf.real.top.cluster["0"], 1, mean.ccf.real.top.cluster["0"], 0.0, 
		length = 0.0, code = 3, angle = 180, col = "#225EA8", lty = 1)

	setwd(metalist[[1]]$Loc); getwd()
	dev.copy(pdf, "Rplot_Randomisations_CCF_CDF.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

# How many randomisations have subclusters with a higher CCF at offset 0 than observed dataset?
Count(which(means.at.lag.0 >= mean.ccf.real.top.cluster["0"])) # Answer (ENA): 2 out of 500 (equivalent to p = 0.004)

# Just for the record: print top CCFs
means.at.lag.0[order(means.at.lag.0, decreasing = TRUE)][1:5]
mean.ccf.real.top.cluster["0"]

	
# -----------------------------------------------------------------------------
# 5. HEATMAP OF A REPESENTATIVE RANDOMISATION:

# First need to trace back where in the null-included list (new.list) the representative
# randomisation originally came from.

#-----------------
# 5a) DATA CLEANUP: Identify the full randomised dataset corresponding to the representative 
# top-correlating subcluster

# Question: if our hit is no.452 in new.list.non.null, which no. does it have in the bigger new.list?
# method: count which is the 452th occurence of non-NULL
# which(notnull == TRUE) is your friend

# inputs: 
# new.list.non.null # only non-NULL lists (only 500 randomisation that lead to fruitful clusters);
#                     this list contains top.cluster for each randomisation but not the full set of 45 filo!
# new.list          # Contains the full stuff (2500 randomisations), but not in clustered format

which(notnull == TRUE)
which(notnull == TRUE)[representative]
representative.match.in.big.list <- which(notnull == TRUE)[representative]
#---------------
# Quality check:
identical(new.list[representative.match.in.big.list], new.list.non.null[representative])
stopifnot(identical(new.list[representative.match.in.big.list], new.list.non.null[representative]))

# From list.ccf.randomised pull the representative dataset
representative.randomisation <- as.data.frame(list.ccf.randomised[representative.match.in.big.list])
representative.rand.move <- as.data.frame(list.all.move.rand[representative.match.in.big.list])

#-----------------
# 5b) Re-plot the original heatmap (using heatmap.2 for having control over colour scale)

library(gplots)

myHeatmap2 <- function(x, ...) {
  map.input = t(x)
  distance <- dist(map.input[, 18:24], method = "euclidean")
  cluster <- hclust(distance, method = "complete")
  heatmap.2(map.input, Rowv = as.dendrogram(cluster), Colv = NA, 
	    xlab = "Lag", 
	  	col = curr.pal, 	
	  	trace = "none",
	  	scale = "none",
	  	key.title = NA, 
	  	key.ylab = NA, 
	  	key.xlab = "CCF value",
	  	density.info = 'none',
#	  	density.info = 'histogram',
	  	...
  		)
 }


scale.edges <- range(ccf.tip.dctm, na.rm = TRUE)
#scale.edges <- c(-0.6, 1)
scale.breaks <- seq(scale.edges[1], scale.edges[2], length.out = length(curr.pal)+1)

dev.new()
myHeatmap2(ccf.tip.dctm, breaks = scale.breaks)

setwd(metalist[[1]]$Loc); getwd()
#	dev.copy(pdf, "Rplot_Heatmap2.pdf", 
	dev.copy(pdf, "Rplot_Heatmap2_no-trace.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()


#-----------------
# 5c) representative randomisation as heatmap:

dev.new()
myHeatmap2(representative.randomisation, breaks = scale.breaks)

setwd(metalist[[1]]$Loc); getwd()
	dev.copy(pdf, "Rplot_Heatmap2_repRandomisation.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

# The range of two datasets:
printEdges <- function(x) print(c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
printEdges(representative.randomisation)
printEdges(ccf.tip.dctm)


#-----------------
# 5d) representative randomisation as XY (z-score); and compare with true data:


list.all.move.rand[representative.match.in.big.list]
list.ccf.randomised[representative.match.in.big.list]
representative.randomisation # CCF values

TCS    <- SearchClusterSpace(ccf.tip.dctm, target.size)$top.cluster.IDs;  TCS
nonTCS <- SearchClusterSpace(ccf.tip.dctm, target.size)$other.IDs;  nonTCS


rep.rand.TCS    <- SearchClusterSpace(representative.randomisation, target.size)$top.cluster.IDs;  rep.rand.TCS
rep.rand.nonTCS <- SearchClusterSpace(representative.randomisation, target.size)$other.IDs;  rep.rand.nonTCS

rep.rand.all.move <- as.data.frame(list.all.move.rand[representative.match.in.big.list])
# rep.rand.all.move

# z.move <- scale(all.move, scale = TRUE, center = TRUE)
# z.tip <- scale(tip.f, scale = TRUE, center = TRUE)
rep.rand.z.move <- scale(rep.rand.all.move, scale = TRUE, center = TRUE)


XY.Subset <- function(xtable, ytable, subset, ...) {
	x1 <- xtable[, subset]
	y1 <- ytable[, subset]
	
	rho <- cor.test(unlist(as.data.frame(x1)), unlist(as.data.frame(y1)), 
		na.action = "na.exclude")$estimate
	
	matplot(x1, y1, ...)
    legend("bottomright", 
    	legend = paste("Pearson R =", signif(rho, 2)), 
    	cex= 1, bty = "n")
    abline(h = 0, lty = 2, col = "grey"); 
    #abline(v = 1, lty = 2, col = "grey")
}


dev.new(height = 3.2, width = 6)
	par(mfrow = c(1,2))
	par (mar = c(4,4,1.2,0.75) + 0.1)




XY.Subset(z.tip, z.move, TCS, 
	col = curr.cols.60[2],
	pch = 16,
	xlab = "Norm. tip fluorescence [z-score]",
	ylab = expression("Tip movement [z-score]")
	, ylim = 1.1 * range(z.move, na.rm = TRUE)
	, xlim = 1.1 * range(z.tip[!is.na(z.move)], na.rm = TRUE)
	)
	abline(v = 0, lty = 2, col = "grey")

XY.Subset(z.tip, z.move, nonTCS, 
	col = curr.cols.60[1],
	pch = 16,
	xlab = "Norm. tip fluorescence [z-score]",
	ylab = ""
	, ylim = 1.1 * range(z.move, na.rm = TRUE)
	, xlim = 1.1 * range(z.tip[!is.na(z.move)], na.rm = TRUE)
	)
	abline(v = 0, lty = 2, col = "grey")

setwd(metalist[[1]]$Loc)
dev.copy(pdf, "Rplot_XY_Clusters_z-score.pdf", width = dev.size()[1], height = dev.size()[2])
dev.off()


XY.Subset(z.tip, rep.rand.z.move, rep.rand.TCS, 
	col = curr.cols.60[2],
	pch = 16,
	xlab = "Norm. tip fluorescence [z-score]",
	ylab = expression("Randomised tip movement [z-score]")
	, ylim = 1.1 * range(z.move, na.rm = TRUE),
#	xlim = c(-3, 5)
	
	xlim = 1.1 * range(z.tip[!is.na(z.move)], na.rm = TRUE)
	)
	abline(v = 0, lty = 2, col = "grey")

XY.Subset(z.tip, rep.rand.z.move, rep.rand.nonTCS, 
	col = curr.cols.60[1],
	pch = 16,
	xlab = "Norm. tip fluorescence [z-score]",
	ylab = "",
	xlim = c(-3, 5)
	, ylim = 1.1 * range(z.move, na.rm = TRUE)
#	, xlim = 1.1 * range(z.tip[!is.na(z.move)], na.rm = TRUE)
	)
	abline(v = 0, lty = 2, col = "grey")

setwd(metalist[[1]]$Loc)
dev.copy(pdf, "Rplot_XY_Clusters_z-score_ReprRandomisation.pdf", width = dev.size()[1], height = dev.size()[2])
dev.off()



# -----------------------------------------------------------------------------
# VISUALISE RANDOMISATION APPROACH:


Lines.TipF.Move <- function(y1 = all.move, y2 = tip.f, 
							subset, legend, legend.where = "topleft", ...) {
	
	cols <- c('#00CC0080', '#33333390', '#33333310')
	
	# For all.move, center around 0
	y1.range = abs(range(y1[, subset], na.rm = TRUE))
	y1greater = y1.range[whichMax(y1.range)]
	ylim1 = c(-y1greater, y1greater)
	
	# For tip.f, center around 1
	y2.range = range(y2[, subset], na.rm = TRUE)
	dist.from.1 = abs(1 - y2.range)
	which.further = whichMax(abs(dist.from.1))
	distance = dist.from.1[which.further]
	ylim2 = c(1 - distance, 1 + distance)
		
	matplot(all.dS[, subset], (y1[, subset]) / spt,
		type = "l",
		lwd = 3,
		col = cols[2],
		ylim = ylim1,
		# ylim = c(-0.32, 0.32)/2,
		xlab = "Time [s]",
		ylab = expression("Tip movement [" * mu * "m / s]"),
		...
		)
	abline(h = 0, lty = 3)	
	abline(h = threshold.ext.per.t / spt, col = "lightgrey", lty = 3)
	abline(h = threshold.retr.per.t / spt, col = "lightgrey", lty = 3)
	par(new = TRUE)
	matplot(all.dS[, subset], y2[, subset], 
		col = cols[1],
		lwd = 3,
		type="l", ann=FALSE, yaxt="n",
		ylim = ylim2
	#	ylim = c(ylo2, yhi2))
	#	ylim = c(0.7, 1.3)
	)	
	axis(4)
	mtext(expression ("Tip Fluorescence"), side=4,line=3, col = cols[1], cex = 1) 	
		# use cex 0.7 if par(mfrow = c(3,3), mar = c(4,4,1,4) + 0.1)) [loop below]
	legend(legend.where, legend = legend, bty = "n")
}




# SAVE PDF PLOTS FOR THE FIRST RANDOMISATION OF i FILOPODIA:

# ...


# SAVE PDF PLOTS FOR i RANDOMISATION OF AN EXAMPLE FILOPODIUM

ID.example <- 5
dev.new(width = 5, height = 3)
	par(mar = c(4,4,1,4) + 0.1)
	w = dev.size()[1] 
	h = dev.size()[2]
	Lines.TipF.Move(all.move, tip.f, ID.example, legend = "Original")
	dev.copy(pdf, "Rplot_Random-vis_00_Original.pdf",width = w, height = h); dev.off()

	for (i in 1:99){
		curr.leg = paste0("Randomisation ", i)
		curr.filename = paste0("Rplot_Random-vis_", i, ".pdf")
		dev.new(width = w, height = h)
			par(mar = c(4,4,1,4) + 0.1)
		Lines.TipF.Move(list.all.move.rand[[i]], tip.f, ID.example, legend = curr.leg)
			dev.copy(pdf, curr.filename, width = w, height = h) 
			dev.off(); graphics.off()
		}

# SAVE PDF PLOTS FOR i HEATMAPS:

dev.new(); myHeatmap2(ccf.tip.dctm, breaks = scale.breaks)
	dev.copy(pdf, "Rplot_Random_Heatmap_00_ORIG.pdf"); dev.off(); graphics.off()

for(i in 1:100) {
	curr.filename = paste0("Rplot_Random_Heatmap_", i, ".pdf")
	dev.new()
	myHeatmap2(
	as.data.frame(list.ccf.randomised# [representative.match.in.big.list])
		[which(notnull == TRUE)] #[representative]
			[i]),
			breaks = scale.breaks)
	dev.copy(pdf, curr.filename); dev.off(); graphics.off()		
}

		
myHeatmap2(representative.randomisation, breaks = scale.breaks)

