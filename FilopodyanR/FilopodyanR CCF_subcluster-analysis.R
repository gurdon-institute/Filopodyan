# TIP FLUORESCENCE & MOVEMENT - subcluster analysis

# This script is part of a suite of scripts for analysis of filopodia dynamics 
# using the Fiji plugin Filopodyan. 
#
# Data input: requires an .Rdata file from the upstream script ('Filopodyan CCF.R') 
# Data output: comparison of properties of filopodia within different subclusters
#
# Downstream applications: Randomisation analysis.

rm(list = ls())

# ---------------------------------------------------------------------------
# Required packages:

# install.packages("Hmisc", dependencies=TRUE, repos="http://cran.rstudio.com/")
# install.packages("RColorBrewer", dependencies=TRUE, repos="http://cran.rstudio.com/")
# install.packages("wavethresh", dependencies=TRUE, repos="http://cran.rstudio.com/")
library(Hmisc)
library(RColorBrewer)
library(wavethresh)
library(tidyr)

# Color palette:
# display.brewer.all()
curr.pal = brewer.pal(11, "RdBu")
curr.cols = curr.pal[c(4, 10, 2)] # "#41B6C4" "#225EA8" "#EDF8B1" for "YlGnBu"
curr.cols.30 = paste0(curr.cols, "30")
curr.cols.60 = paste0(curr.cols, "60")


#---------------------------------------------------------------------------
# 1. Load data from saved workspace


# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01/LastWorkspace_CCFs.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toGC/LastWorkspace_CCFs.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_NormOFF/LastWorkspace_CCFs.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toFilo/LastWorkspace_CCFs.Rdata')

# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01/LastWorkspace_CCFs.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_NormOFF/LastWorkspace_CCFs.Rdata')
 load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC/LastWorkspace_CCFs.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_TOCA/Huang4-01/LastWorkspace_CCFs.Rdata')


# ! BASED ON THE DATASET, SET THE ***GUIDE SUBCLUSTER SIZE*** IN SECTION 3 (line 187?) !

# Check normalisation method:
metalist[[1]]$nor.tip.setting

# Check background correction method:
metalist[[1]]$bg.corr.setting

# Saving location:
metalist[[1]]$Loc <- folder.names[1]


identical(metalist[[1]]$tip.f, metalist[[1]]$all.th.tip.corr.nor2)
identical(metalist[[1]]$tip.f, metalist[[1]]$all.th.tip.corr.nor1)
identical(metalist[[1]]$tip.f, metalist[[1]]$all.th.tip.corrected)


#---------------------------------------------------------------------------
# 2. Custom functions for working with clusters:

Count <- function(x) length(x[!is.na(x)])			 

SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

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
  heatmap(map.input, Rowv = as.dendrogram(cluster), Colv = NA, xlab = "Lag", col = curr.pal, 	scale = "none")	
}

nthSubcluster <- function(x, n.clusters, nth) {
  which(GoCluster(x, n.clusters = n.clusters) == nth)
}
nthSubclusterOthers <- function(x, n.clusters, nth) {
  which(GoCluster(x, n.clusters = n.clusters) != nth)
}

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
whichMin <-  function(x) which(x == min(x))
whichNearest <- function(x, value) {
	dist <- abs(x - value)
	nearest <- whichMin(dist)
	return(nearest)
}
Ordered <- function(x, ...) x[order(x, decreasing = FALSE, ...)]


SizeMaxSubcluster <- function(x, n.clusters = 2) {
  
  # Takes data frame x, returns the size of the subcluster with highest mean at lag = 0 (at specified n clusters)
  #   - Calculate means at lag = 0
  #   - Find subcluster with highest mean at 0
  #   - Return the size of this top subcluster
  
  all.subcl <- ListSubclusters(x, n.clusters)
  clust.means <- lapply(all.subcl, function(x) apply(x["0", ], 1, mean, na.rm = TRUE)) 
  	# This (above) trips single-element subclusters (number of dimensions!)
  top.cluster <- whichMax(unlist(clust.means))
  top.size <- ncol(all.subcl[[top.cluster]])
  return(top.size)  
}

TopSubcluster <- function(x, n.clusters = 2) {
  
  # Takes data frame x, returns top most correlated cluster
  
  all.subcl <- ListSubclusters(x, n.clusters)
  clust.means <- lapply(all.subcl, function(x) apply(x["0", ], 1, mean, na.rm = TRUE)) 
  	# This (above) trips single-element subclusters (number of dimensions!)
  top.cluster <- whichMax(unlist(clust.means))
  output <- list(
    "top.cluster.ID" = top.cluster,
    "top.cluster"   = all.subcl[[top.cluster]]
  )
  return(output)
}

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

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }


#---------------------------------------------------------------------------
# 3. Manually select size of top subcluster based on the heatmap:

dev.new()
myHeatmap(ccf.tip.dctm)


guide.size <- 9  # size of top subcluster in the initial CCF analysis (input manually) 

target.range <- c(0.85, 1.15) * guide.size  # <-- Tolerance range for clusters of randomised data
target.min <- ceiling(target.range[1])
target.max <- floor(target.range[2])
target.size <- (target.min:target.max)
target.size

# For our datasets, guide.size: 
#  ENA: 14  
#  VASP: 9  (very high CCF if 9; slightly lower at 37)


SearchClusterSpace(ccf.tip.dctm, target.size = target.size)
SearchClusterSpace(ccf.tip.dctm, target.size = target.size)$top.cluster.IDs
SearchClusterSpace(ccf.tip.dctm, target.size = target.size)$other.IDs

#---------------------------------------------------------------------------
# 4. Define IDs of the top correlating subcluster and of all other filopodia

TCS    <- SearchClusterSpace(ccf.tip.dctm, target.size)$top.cluster.IDs;  TCS
nonTCS <- SearchClusterSpace(ccf.tip.dctm, target.size)$other.IDs;  nonTCS

# colors by cluster:

TCS
nonTCS

col.by.clust <- rep(NA, ncol(all.move))
	col.by.clust[TCS] <- curr.cols[2]
	col.by.clust[nonTCS] <- curr.cols[1]
col.by.clust.30 <- paste0(col.by.clust, "30")	
col.by.clust.60 <- paste0(col.by.clust, "60")	

#---------------------------------------------------------------------------
# 5. Visualise CCFs of TCS and non-TCS filopodia:

CcfLinePlot_2clust <- function(x, c1, c2, ...) {
	mean.1 <- apply(x[, c1], 1, mean, na.rm = TRUE)
	mean.2 <- apply(x[, c2], 1, mean, na.rm = TRUE)
	ci.1 <- apply(x[, c1], 1, CI)
	ci.2 <- apply(x[, c2], 1, CI)

	#yhi = max(x, na.rm = TRUE)
	#ylo = min(x, na.rm = TRUE)

	#dev.new()		
		matplot(lag.in.s, mean.1,	
			type = "l",
			main = "",
			xlab = "Offset [s]",
			cex.lab = 1.2,
			cex.axis = 1.2,
			ylab = "CCF",
			# col = "#225EA8",
			col = curr.cols[2],
			lwd = 4,
			#ylim = c(ylo, yhi),
			...)
		matplot(lag.in.s, mean.2,	
			add = TRUE,
			type = "l",
			#col = "#7FCDBB",
			col = curr.cols[1],
			lwd = 4,
			...)
		abline(v = 0, col = "black", lty = 3)
		abline(h = 0, col = "black", lwd = 1)	
		ci1 = mean.1 + ci.1
		ci2 = mean.1 - ci.1
		DrawErrorAsPolygon(lag.in.s, ci1, ci2, col = paste0(curr.cols[2], "20"))
		ci1 = mean.2 + ci.2
		ci2 = mean.2 - ci.2
		DrawErrorAsPolygon(lag.in.s, ci1, ci2, col = paste0(curr.cols[1], "20"))
}

dev.new(width = 5, height = 3.5)
	par(mar = c(5,4,1,1)+0.1)
	CcfLinePlot_2clust(ccf.tip.dctm, TCS, nonTCS, 
		 ylim = c(-0.1, 0.85)
		# ylim = c(-0.1, 0.65)
		)

	setwd(metalist[[1]]$Loc); getwd()
	dev.copy(pdf, "Rplot_Lines_CCF.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()



# getWeights <- function(x) {colSums(!is.na(x))}
# getWeights(all.move)
# weights.vec <- getWeights(all.move)	

# CcfLinePlot_2clust_Weighted <- function(x, c1, c2, ...) {

	# w.mean.1 <- apply(x[, c1], 1, weighted.mean, w = weights.vec[c1], na.rm = TRUE)
	# w.mean.2 <- apply(x[, c2], 1, weighted.mean, w = weights.vec[c2], na.rm = TRUE)
	# w.sd.1 <- sqrt( apply(x[, c1], 1, wtd.var, w = weights.vec[c1], na.rm = TRUE)) 
	# w.sd.2 <- sqrt( apply(x[, c2], 1, wtd.var, w = weights.vec[c2], na.rm = TRUE)) 
	# count.1 <- apply(x[, c1], 1, Count)
	# count.2 <- apply(x[, c2], 1, Count)
	# w.ci.1 <- 1.96 * w.sd.1 / sqrt(count.1)
	# w.ci.2 <- 1.96 * w.sd.2 / sqrt(count.2)
	
	# #yhi = max(x, na.rm = TRUE)
	# #ylo = min(x, na.rm = TRUE)
	
	# #dev.new()		
		# matplot(lag.in.s, w.mean.1,	
			# type = "l",
			# main = "",
			# xlab = "Lag [s]",
			# cex.lab = 1.2,
			# cex.axis = 1.2,
			# ylab = "CCF (Tip Fluorescence & Movement)",
			# col = "#225EA8",
			# lwd = 4,
			# ...)
		# matplot(lag.in.s, w.mean.2,	
			# add = TRUE,
			# type = "l",
			# col = "#7FCDBB",
			# lwd = 4,
			# ...)
		# abline(v = 0, col = "black", lty = 3)
		# abline(h = 0, col = "black", lwd = 1)	
		# ci1 = w.mean.1 + w.ci.1
		# ci2 = w.mean.1 - w.ci.1
		# DrawErrorAsPolygon(lag.in.s, ci1, ci2, col = "#225EA820")
		# ci1 = w.mean.2 + w.ci.2
		# ci2 = w.mean.2 - w.ci.2
		# DrawErrorAsPolygon(lag.in.s, ci1, ci2, col = "#7FCDBB20")
			
# }

# CcfLinePlot_2clust_Weighted(ccf.tip.dctm, TCS, nonTCS, ylim = c(-0.2, 0.6), add = TRUE)

#---------------------------------------------------------------------------
# 5. Visualise XY plots of TCS and non-TCS filopodia 
# (fluorescence and movement, raw and z-score):

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

# Plot XY values, directly: 

dev.new(height = 3.2, width = 6)
	par(mfrow = c(1,2))
	par (mar = c(4,4,1.2,0.75) + 0.1)

XY.Subset(tip.f, all.move/spt, TCS, 
	col = curr.cols.60[2],
	pch = 16,
	xlab = "Tip fluorescence [au]",
	ylab = expression("Tip movement [" * mu * "m / s]")
	, ylim = range(all.move/2, na.rm = TRUE)
	, xlim = range(tip.f[!is.na(all.move)], na.rm = TRUE)
	)
	abline(v = 1, lty = 2, col = "grey")

XY.Subset(tip.f, all.move/spt, nonTCS, 
	col = curr.cols.60[1],
	pch = 16,
	xlab = "Tip fluorescence [au]",
	ylab = ""
	, ylim = range(all.move/spt, na.rm = TRUE)
	, xlim = range(tip.f[!is.na(all.move)], na.rm = TRUE)
	)
	abline(v = 1, lty = 2, col = "grey")

setwd(metalist[[1]]$Loc)
dev.copy(pdf, "Rplot_XY_Clusters.pdf", width = dev.size()[1], height = dev.size()[2])
dev.off()

# Plot XY values, as z-scores: 

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
	ylab = expression("Tip movement [z-score]")
	, ylim = 1.1 * range(z.move, na.rm = TRUE)
	, xlim = 1.1 * range(z.tip[!is.na(z.move)], na.rm = TRUE)
	)
	abline(v = 0, lty = 2, col = "grey")

setwd(metalist[[1]]$Loc)
dev.copy(pdf, "Rplot_XY_Clusters_z-score.pdf", width = dev.size()[1], height = dev.size()[2])
dev.off()


#---------------------------------------------------------------------------
# 6. Select most representative TCS and non-TCS filopodia

# selection criterion 1: sufficient n.timepoints > median(n.timepoints)
# selection criterion 2: mean CCF close to its cluster mean

# visualise n.timepoints:

n.timepoints <- colSums( !is.na(all.move)); n.timepoints  

dev.new(width = 3, height = 3)
	par(mar = c(1,4,1,1)+0.1)
	stripchart(n.timepoints, pch = 21, method = "jitter", vertical = TRUE, 	
		bg = paste0(col.by.clust, "80"),
		ylab = "Number of timepoints")
	boxplot(n.timepoints, add = TRUE, border = "grey")

	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_nTimepoints.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()


ClosestMatch <- function(dataset, subset, threshold) {
	
	# Identifies filopodium with the CCF closest to the mean of its cluster,
	# filtered for those with sufficiently long time series.
	
	# Find subset mean at offset zero
	ccf.at.0 <- unlist(ccf.tip.dctm["0", ])
	ccf.at.0.subset.mean <- mean(ccf.at.0[subset])

	# Distances from subset mean:
	distance <- abs(ccf.at.0 - ccf.at.0.subset.mean)

	# Order by distance  #Ordered(distance)
	order.distance <- order(distance)
	
	# Generate thresholded n.timepoints
	threshold = threshold
	#threshold = median(n.timepoints, na.rm = TRUE)
	sufficient.length <-  which(n.timepoints > threshold)

	# (ordered list) present in sufficient length
	order.distance %in% sufficient.length
	FirstTrue <- function(x) min(which(x == TRUE))
	nthTrue <- function(x, n) which(x == TRUE)[n]
		
	FirstTrue(order.distance %in% sufficient.length)
	closest.match <- order.distance[	FirstTrue(order.distance %in% sufficient.length) ]
	second.match  <- order.distance[nthTrue(order.distance %in% sufficient.length, 2)]
	third.match   <- order.distance[nthTrue(order.distance %in% sufficient.length, 3)]
	fourth.match   <- order.distance[nthTrue(order.distance %in% sufficient.length, 4)]
	fifth.match   <- order.distance[nthTrue(order.distance %in% sufficient.length, 5)]
	sixth.match   <- order.distance[nthTrue(order.distance %in% sufficient.length, 6)]	
	
	# return ID (in original table) of the closest match
	return(list("closest.match" = closest.match, 
				"second.match" = second.match,
				"third.match" = third.match,
				"fourth.match" = fourth.match,
				"fifth.match" = fifth.match,
				"sixth.match" = sixth.match))
}

rep.TCS    <- ClosestMatch(ccf.tip.dctm, TCS, median(n.timepoints, na.rm = TRUE))
rep.nonTCS <- ClosestMatch(ccf.tip.dctm, nonTCS, median(n.timepoints, na.rm = TRUE))


#---------------------------------------------------------------------------
# 7. Plots for individual filopodia:

# Six representative filo from each cluster:
# (in order of closeness to cluster mean)

XY.Subset.call <- function(x, y, ID, legend.where = "topright", ...) {
	XY.Subset(x, y, ID,  
#	col = curr.cols.30[2],
	pch = 16,
	xlab = "Tip fluorescence [au]",
	ylab = expression("Tip movement [" * mu * "m / s]")
	, ylim = range(all.move/2, na.rm = TRUE)
	, xlim = range(tip.f[!is.na(all.move)], na.rm = TRUE),
	...
	)
	legend(legend.where, colnames(all.move)[ID], bty = "n")
	abline(v = 1, lty = 2, col = "grey")
}

# From TCS
dev.new(height = 2.6, width = 8)
	par(mfrow = c(1,3))
	par (mar = c(4,4,1.2,1) + 0.1)
	XY.Subset.call(tip.f, all.move/spt, ID = rep.TCS$closest.match, col = curr.cols.30[2])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.TCS$second.match, col = curr.cols.30[2])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.TCS$third.match, col = curr.cols.30[2])

	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_XY_TCS_rep1-3.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()

dev.new(height = 2.6, width = 8)
	par(mfrow = c(1,3))
	par (mar = c(4,4,1.2,1) + 0.1)
	XY.Subset.call(tip.f, all.move/spt, ID = rep.TCS$fourth.match, col = curr.cols.30[2])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.TCS$fifth.match, col = curr.cols.30[2])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.TCS$sixth.match, col = curr.cols.30[2])

	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_XY_TCS_rep4-6.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()


# From nonTCS
dev.new(height = 2.6, width = 8)
	par(mfrow = c(1,3))
	par (mar = c(4,4,1.2,1) + 0.1)
	XY.Subset.call(tip.f, all.move/spt, ID = rep.nonTCS$closest.match, col = curr.cols.30[1])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.nonTCS$second.match, col = curr.cols.30[1])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.nonTCS$third.match, col = curr.cols.30[1])

	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_XY_nonTCS_rep1-3.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()

dev.new(height = 2.6, width = 8)
	par(mfrow = c(1,3))
	par (mar = c(4,4,1.2,1) + 0.1)
	XY.Subset.call(tip.f, all.move/spt, ID = rep.nonTCS$fourth.match, col = curr.cols.30[1])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.nonTCS$fifth.match, col = curr.cols.30[1])
	XY.Subset.call(tip.f, all.move/spt, ID = rep.nonTCS$sixth.match, col = curr.cols.30[1])

	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_XY_nonTCS_rep4-6.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()


# 7a. XY scatterplots:

# A chosen representative from the top-corresponding cluster:

dev.new(height = 3.2, width = 6)
	par(mfrow = c(1,2))
	par (mar = c(4,4,1.2,1) + 0.1)
	
XY.Subset(tip.f, all.move/spt, rep.TCS$third.match,  
	col = curr.cols.30[2],
	pch = 16,
	xlab = "Norm. tip fluorescence [au]",
	ylab = expression("Tip movement [" * mu * "m / s]")
	, ylim = range(all.move/2, na.rm = TRUE)
	, xlim = range(tip.f[!is.na(all.move)], na.rm = TRUE)
	)
	abline(v = 1, lty = 2, col = "grey")


# Representative from the non-top-corresponding cluster (use closest.match, second.match, third.match to see several):

XY.Subset(tip.f, all.move/spt, rep.nonTCS$closest.match, 
	col = curr.cols.30[1],
	pch = 16,
	xlab = "Norm. tip fluorescence [au]",
	ylab = expression("Tip movement [" * mu * "m / s]")
	, ylim = range(all.move/2, na.rm = TRUE)
	, xlim = range(tip.f[!is.na(all.move)], na.rm = TRUE)
	)
	abline(v = 1, lty = 2, col = "grey")

	# setwd(metalist[[1]]$Loc)
	# dev.copy(pdf, "Rplot_XY_Cluster_representatives.pdf", width = dev.size()[1], height = dev.size()[2])
	# dev.off()


# Top most positively correlating filopodium: 
ccf.at.0 <- unlist(ccf.tip.dctm["0", ])
XY.Subset(tip.f, all.move/spt, order(ccf.at.0, decreasing = TRUE)[1], 
	col = curr.cols.30[2],
	pch = 16,
	xlab = "Norm. tip fluorescence [au]",
	ylab = expression("Tip movement [" * mu * "m / s]")
	, ylim = range(all.move/2, na.rm = TRUE)
	, xlim = range(tip.f[!is.na(all.move)], na.rm = TRUE)
	)
	abline(v = 1, lty = 2, col = "grey")

	# setwd(metalist[[1]]$Loc)
	# dev.copy(pdf, "Rplot_XY_TCS_top-filo.pdf", width = dev.size()[1], height = dev.size()[2])
	# dev.off()


# 7b. Line plots for movement and fluorescence:

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
	mtext(expression ("Tip Fluorescence"), side=4,line=3, col = cols[1], cex = 0.8) 	
		# use cex 0.7 if par(mfrow = c(3,3), mar = c(4,4,1,4) + 0.1)) [loop below]
	legend(legend.where, legend = legend, bty = "n")
}

# For individual plots:
#	dev.new(width = 5, height = 3)
#		par(mar = c(4,4,1,4) + 0.1)
	
	# e.g.:
#	Lines.TipF.Move(all.move, tip.f, whichMax(ccf.at.0))
	#Lines.TipF.Move(all.move, tip.f, nonTCS[7])
	#Lines.TipF.Move(all.move, tip.f, rep.TCS$closest.match)
	#Lines.TipF.Move(all.move, tip.f, rep.TCS$second.match)


# Plotting 6 representative filopodia from TCS:
dev.new(width = 6, height = 5)
	par(mar = c(4,4,1,4) + 0.1)
	par(mfrow = c(3, 2))

	for(i in 1:6) {
		try(Lines.TipF.Move(all.move, tip.f, rep.TCS[[i]], 
				legend = colnames(all.move)[rep.TCS[[i]]]))
	}	
	
	setwd(metalist[[1]]$Loc); getwd()
	dev.copy(pdf, "Rplot_ParallelLines_TCS_rep1-6.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

# Plotting 6 representative filopodia from nonTCS:
dev.new(width = 6, height = 5)
	par(mar = c(4,4,1,4) + 0.1)
	par(mfrow = c(3, 2))

	for(i in 1:6) {
		try(Lines.TipF.Move(all.move, tip.f, rep.nonTCS[[i]], 
				legend = colnames(all.move)[rep.nonTCS[[i]]]))
	}	
	
	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_ParallelLines_nonTCS_rep1-6.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()


# For very many plots:

	for (i in 1:ncol(all.move)) {
		# make a new plotting window for every 9th filopodium
		if(i %% 9 == 1)	{
			dev.new(height = 6, width = 9); 
				par(mfrow = c(3,3)); 
				par(mar = c(4,4,1,4) + 0.1)}
		try(Lines.TipF.Move(all.move, tip.f, i))
		legend("topleft", colnames(all.move)[i], bty = "n", text.col = col.by.clust[i])
		if(i %% 9 == 0) {
			setwd(metalist[[1]]$Loc)
			dev.copy(pdf, paste0("Rplot_ParallelLines_ALL-batch", i, ".pdf"), 
				width = dev.size()[1], height = dev.size()[2])
			dev.off()
		}
	}
		# save last one:
		dev.copy(pdf, paste0("Rplot_ParallelLines_ALL-batch", "Z", ".pdf"), 
				width = dev.size()[1], height = dev.size()[2])
		dev.off()

graphics.off()

#---------------------------------------------------------------------------
# 8. variance(movement) vs variance(fluorescence) by cluster

var.all.move <- apply(all.move/spt, 2, var, na.rm = TRUE)
var.tip.f <- apply(tip.f, 2, var, na.rm = TRUE)

dev.new(height = 3.2, width = 3.2)
	par(mar = c(5,4,1,1)+0.1)
	plot(var.tip.f, var.all.move, 
		ylab = "Variance(Movement)",
		xlab = "Variance(Tip fluorescence)",
		xlim = range(var.tip.f, na.rm = TRUE) * 1.2,
		ylim = range(var.all.move, na.rm = TRUE) * 1.2,
		pch = 16,
		col = paste0(col.by.clust, "50")
	)

dev.new(height=3.5, width = 3.5)

	zones = matrix(c(2,0,1,3), ncol = 2, byrow = 3)
	layout(zones, widths = c(4/5, 1/5), heights = c(1/5,4/5))
	
	curr.xlim = range(var.tip.f, na.rm = TRUE) * 1.2
	curr.ylim = range(var.all.move, na.rm = TRUE) * 1.2
	
	par(mar = c(4,4,0,0)+0.1)
	par(bty = "n")
	plot(var.tip.f, var.all.move, 
			ylab = "variance(Tip movement)",
			xlab = "variance(Tip fluorescence)",
			xlim = curr.xlim,
			ylim = curr.ylim,
			pch = 16,
			col = paste0(col.by.clust, "70")
		)
		
	par(mar = c(0,4,0,0)+0.1)
	par(bty = "n", xaxt = "n", yaxt = "n")
	boxplot(var.tip.f[TCS], horizontal = TRUE, ylim = curr.xlim, boxwex = 0.4,
		border=curr.cols[2])
	boxplot(var.tip.f[nonTCS], horizontal = TRUE, ylim = curr.xlim, boxwex = 0.4,
		border=curr.cols[1],
		add= TRUE, at = 0.7)

	par(mar = c(4,0,0,0)+0.1)
	par(bty = "n", xaxt = "n", yaxt = "n")
	boxplot(var.all.move[TCS], horizontal = FALSE, ylim = curr.ylim, boxwex = 0.4,
		border=curr.cols[2])
	boxplot(var.all.move[nonTCS], horizontal = FALSE, ylim = curr.ylim, boxwex = 0.4,
		border=curr.cols[1],
		add= TRUE, at = 0.7)

	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_XY_byCluster_Variance.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()


# 8b) median(fluorescence) vs median(movement) by cluster

med.all.move <- apply(all.move/spt, 2, median, na.rm = TRUE)
med.tip.f <- apply(tip.f, 2, median, na.rm = TRUE)


dev.new(height=3.5, width = 3.5)

	zones = matrix(c(2,0,1,3), ncol = 2, byrow = 3)
	layout(zones, widths = c(4/5, 1/5), heights = c(1/5,4/5))
	
	curr.xlim = range(med.tip.f, na.rm = TRUE) * 1.2
	curr.ylim = range(med.all.move, na.rm = TRUE) * 1.2
	
	par(mar = c(4,4,0,0)+0.1)
	par(bty = "n")
	plot(med.tip.f, med.all.move, 
			ylab = "median(Tip movement)",
			xlab = "median(Tip fluorescence)",
			xlim = curr.xlim,
			ylim = curr.ylim,
			pch = 16,
			col = paste0(col.by.clust, "70")
		)
		
	par(mar = c(0,4,0,0)+0.1)
	par(bty = "n", xaxt = "n", yaxt = "n")
	boxplot(med.tip.f[TCS], horizontal = TRUE, ylim = curr.xlim, boxwex = 0.4,
		border=curr.cols[2])
	boxplot(med.tip.f[nonTCS], horizontal = TRUE, ylim = curr.xlim, boxwex = 0.4,
		border=curr.cols[1],
		add= TRUE, at = 0.7)

	par(mar = c(4,0,0,0)+0.1)
	par(bty = "n", xaxt = "n", yaxt = "n")
	boxplot(med.all.move[TCS], horizontal = FALSE, ylim = curr.ylim, boxwex = 0.4,
		border=curr.cols[2])
	boxplot(med.all.move[nonTCS], horizontal = FALSE, ylim = curr.ylim, boxwex = 0.4,
		border=curr.cols[1],
		add= TRUE, at = 0.7)

	setwd(metalist[[1]]$Loc)
	dev.copy(pdf, "Rplot_XY_byCluster_Medians.pdf", width = dev.size()[1], height = dev.size()[2])
	dev.off()


#---------------------------------------------------------------------------
# 9. Plot morphodynamic parameters of TCS vs non-TCS filopodia
# Be careful with those filopodia that have been excluded becasue of n.timepoints < 17 [!]
# (use name matching to avoid mishaps?) 

rm(list = ls())
#load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC/LastWorkspace_CCF_Subclusters.Rdata')
 load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toGC/LastWorkspace_CCF_Subclusters.Rdata')

# Loc.save <- "/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC"


setwd(Loc.save)
TCS
nonTCS


# create function Tidy
# works as: Tidy(metalist[[1]], ind = list(TCS, nonTCS), names = c("TCS", "nonTCS")) -> produces a tidy dataset

# Tidy(data, vector.index, names, ...)

# simpler version that works for two (use and maintain generic version, below):
Tidy <- function(data, indices.as.list, names, var.name) {
	
	stopifnot(length(indices.as.list) == length(names))
	
	# sort out indices
	ind1 <-	indices.as.list[[1]]
	ind2 <- indices.as.list[[2]]
	
	# lengths:
	length1 <- length(ind1)
	length2 <- length(ind2)
	
	# output table
	df <- data.frame(
		"value" = c(data[ind1], data[ind2]),
		"source" = c(rep(names[1], length1), rep(names[2], length2))
		)
	
	colnames(df) <- c(var.name, "Category")
	
	return(df)
}

iTidy <- function(data, indices.as.list, names, var.name) {
	
	stopifnot(length(indices.as.list) == length(names))
	
	# sort out indices
	#ind1 <-	indices.as.list[[1]]
	#ind2 <- indices.as.list[[2]]
	
	# lengths:
	#length1 <- length(ind1)
	#length2 <- length(ind2)
	
	# output table
	df <- data.frame("value" = NULL, "source" = NULL)

	for(i in 1:length(names)) {
		length.i <- length(indices.as.list[[i]])
		df.i <- data.frame("value" = data[indices.as.list[[i]]],
						   "source" = rep(names[i], length.i))
		df <- rbind(df, df.i)
	}

	colnames(df) <- c(var.name, "Category")
	
	return(df)
}

# iTidy(metalist[[1]]$max.lengths,	
	# indices.as.list = list(c(1,2,3), c(4,5)),
	# names = c("1-3", "4-5"),
	# var.name = "max.length")
	
# Tidy(metalist[[1]]$max.lengths,	
	# indices.as.list = list(c(1,2,3), c(4,5)),
	# names = c("1-3", "4-5"),
	# var.name = "max.length")

matchNames(TCS, metalist[[1]]$all.length)

matchNames <- function(given.subset, orig.dataset) {
	
	subset.names <- names(given.subset)
	
	#print(subset.names)
	
	if(is.data.frame(orig.dataset)) {
		orig.names <- colnames(orig.dataset)
	} else if(is.vector(orig.dataset)){
		orig.names <- names(orig.dataset)
	}
	
	#print(orig.names)
	
	indices.in.orig <- c()
	for(i in seq_along(subset.names)){
		name.i  <- subset.names[i]
		match.i <- which(orig.names == name.i)
		indices.in.orig <- append(indices.in.orig, match.i)
	}
	
	return(indices.in.orig)
}

# Problem: names contain not only ID but also "Length" or "DCTM"...
# i.e. can only match DCTM from filtered all.move with DCTM from metalist all.dctm table
# however, these same indices should (in principle) be valid for all other metalist data
# frames can check this with ncol for safety

# i.e. these are the matching indices to translate TCS to pre-filtered metalist indices: 
match.TCS <- matchNames(TCS, metalist[[1]]$all.dctm) 
match.nonTCS <- matchNames(nonTCS, metalist[[1]]$all.dctm) 

# QUALITY CONTROL: 
# Test equality of tidy datasets
checkEquality <- function(x, y) {
	plot(unlist(x), unlist(y), pch = 16)
}
checkEquality(
	x = metalist[[1]]$tip.f[, match.TCS],
	y = tip.f[, TCS])
checkEquality(
	x = metalist[[1]]$all.dS[, match.TCS],
	y = all.dS[, TCS])
# or: 
identical(metalist[[1]]$all.dS[, match.TCS],
	all.dS[, TCS])


#-----------------------------
tidy_MaxL <- iTidy(metalist[[1]]$max.lengths, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "max.length")
# library(yarrr)
# pirateplot(tip.f ~ Category, tidy_MaxL) # THIS WORKS!!! 
									# (but only for single-var representations from metalist, not 										# whole tables, clearly!)

# Straightness at max (over 5):
tidy_StraightAtMax5 <- iTidy(metalist[[1]]$straightness.at.max.over5, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "straightness.at.max.over5")
#pirateplot(straightness.at.max.over5 ~ Category, tidy_StraightAtMax5)
#pirateplot(straightness.at.max.over5 ~ Category, tidy_StraightAtMax5, ylim = c(0.76, 1))

tidy_MaxL <- iTidy(metalist[[1]]$max.lengths, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "max.length")

metalist[[1]]$all.time.ext

#----------
# Max length:
 "max.lengths"

tidy_max.lengths <- iTidy(metalist[[1]]$max.lengths, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "max.length")


# Tip movement:
 "med.rate.extens" #/ spt
 "med.rate.retr" #/ spt
 
tidy_med.rate.extens <- iTidy(metalist[[1]]$med.rate.extens / spt, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "med.rate.extens")
 
tidy_med.rate.retr <- iTidy(metalist[[1]]$med.rate.retract / spt * -1, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "med.rate.retract")

#----------
# Straightness at max (over 5):
 "straightness.at.max.over5"
tidy_straightness.at.max.over5 <- iTidy(metalist[[1]]$straightness.at.max.over5, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "straightness.at.max.over5")

# Base movement:
 "med.fdcbm.invas"  #/ spt
 "med.fdcbm.retr"  #/ spt

tidy_med.fdcbm.invas <- iTidy(metalist[[1]]$med.fdcbm.invas / spt, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "med.fdcbm.invas")
tidy_med.fdcbm.retr <- iTidy(metalist[[1]]$med.fdcbm.retr / spt * -1, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "med.fdcbm.retr")
 
#----------
# Tip state: 
 "all.time.ext"
 "all.time.retr"
 "all.time.stall"
 
tidy_all.time.ext <- iTidy(metalist[[1]]$all.time.ext, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "all.time.ext")
tidy_all.time.retr <- iTidy(metalist[[1]]$all.time.retr, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "all.time.ext")
tidy_all.time.stall <- iTidy(metalist[[1]]$all.time.stall, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "all.time.ext") 
 
#----------
# Base state: 
 "all.time.base.inv"
 "all.time.base.retr"
 "all.time.base.stable"

tidy_all.time.base.inv <- iTidy(metalist[[1]]$all.time.base.inv, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "all.time.base.inv")
tidy_all.time.base.retr <- iTidy(metalist[[1]]$all.time.base.retr, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "all.time.base.retr")
tidy_all.time.base.stable <- iTidy(metalist[[1]]$all.time.base.stable, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "all.time.base.stable") 
 
#----------
# Tip persistence (fdctm): 
 "acf.fdctm.roots"  #* spt # (in s)

tidy_acf.fdctm.roots <- iTidy(metalist[[1]]$acf.fdctm.roots * spt, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "acf.fdctm.roots") 

# Tip movement variance: 
tidy_var.all.move  <- iTidy(var.all.move,
				list(TCS, nonTCS),
				c("TCS", "nonTCS"),
				var.name = "var.all.move")


# N timepoints:
tidy_n.timepoints  <- iTidy(n.timepoints,
				list(TCS, nonTCS),
				c("TCS", "nonTCS"),
				var.name = "n.timepoints")
 
#----------
# Fluorescence (tip, filo, body) - 1) raw 2) bg corrected

# 1) raw:

	mean.f.proj.raw <- apply(metalist[[1]]$all.proj, 2, mean, na.rm = TRUE) 
	mean.f.body.raw <- apply(metalist[[1]]$all.body, 2, mean, na.rm = TRUE) 
	mean.f.tip.raw <- apply(metalist[[1]]$all.th.tip, 2, mean, na.rm = TRUE)

tidy_mean.f.proj.raw <- iTidy(mean.f.proj.raw, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.proj.raw") 
tidy_mean.f.body.raw <- iTidy(mean.f.body.raw, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.body.raw") 
tidy_mean.f.tip.raw <- iTidy(mean.f.tip.raw, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.tip.raw") 

# 1) bg-corrected:

	all.proj.corrected = metalist[[1]]$all.proj - metalist[[1]]$all.bg.boundary
	mean.f.proj <- apply(all.proj.corrected, 2, mean, na.rm = TRUE) 
	mean.f.body <- apply(metalist[[1]]$all.body.corrected, 2, mean, na.rm = TRUE) 
	mean.f.tip <- apply(metalist[[1]]$all.th.tip.corrected, 2, mean, na.rm = TRUE)

# 2) bg-corrected & normalised	 

	### Normalised f.proj:  metalist[[1]]$all.proj.corr.nor
	### Normalised f.tip:   metalist[[1]]$all.th.tip.corr.nor2

	mean.f.proj.nor <- apply(metalist[[1]]$all.proj.corr.nor, 2, mean, na.rm = TRUE)
	mean.f.tip.nor <- apply(metalist[[1]]$all.th.tip.corr.nor2, 2, mean, na.rm = TRUE)

tidy_mean.f.proj <- iTidy(mean.f.proj, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.proj") 
tidy_mean.f.body <- iTidy(mean.f.body, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.body") 
tidy_mean.f.tip <- iTidy(mean.f.tip, 
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.tip") 
tidy_mean.f.proj.nor <- iTidy(mean.f.proj.nor,
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.proj.nor")
tidy_mean.f.tip.nor <- iTidy(mean.f.tip.nor,
				list(match.TCS, match.nonTCS), 
				c("TCS", "nonTCS"), 
				var.name = "mean.f.tip.nor")


#----------
# TipF persistence

# required:
	AcfTable <- metalist[[1]]$AcfTable
	FirstNegative <- metalist[[1]]$FirstNegative
	MaxLag <- 120

acf.tip.f <- AcfTable(tip.f, 120)  # calculated from this script (not in metalist)
acf.tip.f.roots <- apply (acf.tip.f, 2, FirstNegative)
	
tidy_acf.tip.f.roots <- iTidy(acf.tip.f.roots * spt, 	
		list(TCS, nonTCS),  # Indices different from match.TCS & match.nonTCS because 
							# the data is coming from this script, not from loaded metalist
		names = c("TCS", "nonTCS"), 
		var.name = "acf.tip.f.roots")

#----------
# TipF variance 
tidy_var.tip.f <- iTidy(var.tip.f,
				list(TCS, nonTCS),
				c("TCS", "nonTCS"),
				var.name = "var.tip.f")

# Split up XY scatterplots into (while extending) and (while retracting)


#-----------------------------	 
# Boxplot function:
curr.cols
Boxplot <- function(x, col = curr.cols[c(2,1,3)], ...) {
  # Boxplot <- function(col = c("grey80", "grey90", "grey70"), ...) {
  
  colnames(x) <- c("value", "source")
  
  #dev.new()
  ylo = min(x$value, na.rm = TRUE)
  yhi = 1.1 * max(x$value, na.rm = TRUE)
  boxplot(value ~ source, data = x,
          outpch = NA,
          border = col,
          col = paste0(col, "20"),
          #  main = curr.title,
          # ylab = curr.Ylab,
          # ylim = c(ylo, yhi),
          ...
  )          
  stripchart(value ~ source, data = x,
             add = TRUE,
             vertical = TRUE,
             method = "jitter",
             # pch = 21, cex = 1,
             # bg = paste0(col, "20"),
             # col = paste0(col, "80")  
             pch = 16, cex = 1,
             col = paste0(col, "40")
  )
  
  mw.p <- wilcox.test(value ~ source, x)$p.value
  legend("topright", legend = paste("Mann-Whitney P =", signif(mw.p, 2)), bty = "n")
}

#--------------------------------------------------------------------------------------- 
# PLOT AND SAVE BOXPLOTS OF FILOPODIUM PARAMETERS:

dev.new(width = 8, height = 2.25)
	par(mfrow = c(1,3))
	par(mar = c(4,4,1,1)+0.1)
	w = dev.size()[1]
	h = dev.size()[2]
setwd(metalist[[1]]$Loc); getwd()

Boxplot(tidy_max.lengths, ylab = expression("Max length [" * mu * "m]"))
Boxplot(tidy_med.rate.extens, ylab = expression("Median tip extension [" * mu * "m/s]"))
Boxplot(tidy_med.rate.retr, ylab = expression("Median tip retraction [" * mu * "m/s]"))
# Save:
	dev.copy(pdf, "Rplot_Box_byCluster_01.pdf", width = w, height = h); dev.off()

dev.new(width = w, height = h); par(mfrow = c(1,3), mar = c(4,4,1,1)+0.1)
Boxplot(tidy_straightness.at.max.over5, ylab = "Straightness at max length [au]")
Boxplot(tidy_med.fdcbm.invas, ylab = expression("Median base invasion [" * mu * "m/s]"))
Boxplot(tidy_med.fdcbm.retr, ylab = expression("Median base retraction [" * mu * "m/s]"))
	dev.copy(pdf, "Rplot_Box_byCluster_02.pdf", width = w, height = h); dev.off()

dev.new(width = w, height = h); par(mfrow = c(1,3), mar = c(4,4,1,1)+0.1)	
Boxplot(tidy_all.time.ext, ylab = "Time extending [fraction of total]")
Boxplot(tidy_all.time.retr, ylab = "Time retracting [fraction of total]")
Boxplot(tidy_all.time.stall, ylab = "Time stalling [fraction of total]")
	dev.copy(pdf, "Rplot_Box_byCluster_03.pdf", width = w, height = h); dev.off()
	
dev.new(width = w, height = h); par(mfrow = c(1,3), mar = c(4,4,1,1)+0.1)
Boxplot(tidy_all.time.base.inv, ylab = "Time base invading [fraction of total]")
Boxplot(tidy_all.time.base.retr, ylab = "Time base retracting [fraction of total]")
Boxplot(tidy_all.time.base.stable, ylab = "Time base stable [fraction of total]")
	dev.copy(pdf, "Rplot_Box_byCluster_04.pdf", width = w, height = h); dev.off()
	
dev.new(width = w, height = h); par(mfrow = c(1,3), mar = c(4,4,1,1)+0.1)
Boxplot(tidy_acf.fdctm.roots, ylab = "Tip persistence [s]")
Boxplot(tidy_var.all.move, ylab = expression("Variance (tip movement) [" * mu * "m/s]"))
Boxplot(tidy_n.timepoints, ylab = "Number of timepoints")
	dev.copy(pdf, "Rplot_Box_byCluster_05.pdf", width = w, height = h); dev.off()
	
dev.new(width = w, height = h); par(mfrow = c(1,3), mar = c(4,4,1,1)+0.1)
Boxplot(tidy_acf.tip.f.roots, ylab = "Persistence in tip fluorescence [s]")
Boxplot(tidy_var.tip.f, ylab = expression("Variance (tip fluorescence) [au]"))
	dev.copy(pdf, "Rplot_Box_byCluster_06.pdf", width = w, height = h); dev.off()

# Fluorescence parameters:
dev.new(width = w, height = h); par(mfrow = c(1,3), mar = c(4,4,1,1)+0.1)
Boxplot(tidy_mean.f.proj.raw, ylab = "F(filopodium, raw) [au]")  
Boxplot(tidy_mean.f.body.raw, ylab = "F(body, raw) [au]")  #
Boxplot(tidy_mean.f.tip.raw, ylab = "F(tip, raw) [au]")  #
	dev.copy(pdf, "Rplot_Box_byCluster_07.pdf", width = w, height = h); dev.off()

dev.new(width = w, height = h); par(mfrow = c(1,3), mar = c(4,4,1,1)+0.1)
Boxplot(tidy_mean.f.proj, ylab = "F(filopodium) [au]")  # bg-corrected
Boxplot(tidy_mean.f.body, ylab = "F(body) [au]")  # bg-corrected 
Boxplot(tidy_mean.f.tip, ylab = "F(tip) [au]")   # bg-corrected
	dev.copy(pdf, "Rplot_Box_byCluster_08.pdf", width = w, height = h); dev.off()

graphics.off()


# Adjusting for multiple comparisons:

# ps = c(0.05, 0.03, 0.0001, 0.43, 0.32, 0.74, 0.02)
# p.adjust(ps, method = "holm")
# p.adjust(ps, method = "bonferroni")

# BONFERRONI SIGNIFICANE THRESHOLD - very conservative: 
# 0.05 / 23 (n.comparisons) = 0.00217


myquantnames <- c(
	"max.lengths",
	"med.rate.extens",
	"med.rate.retr",
	"straightness.at.max.over5",
	"med.fdcbm.invas",
	"med.fdcbm.retr",
	"all.time.ext",
	"all.time.retr",
	"all.time.stall",
	"all.time.base.inv",
	"all.time.base.retr",	
	"all.time.base.stable",
	"acf.fdctm.roots",
	"var.all.move",
	'n.timepoints',
	"acf.tip.f.roots",
	"var.tip.f",
	"mean.f.proj.raw",
	"mean.f.body.raw",
	'mean.f.tip.raw',
	'mean.f.proj',
	"mean.f.body",
	"mean.f.tip"
)
myquant_abbrev <- c(
	"Len",
	"TExt",
	"TRet",
	"Str",
	"BInv",
	"BRet",
	"TTE",
	"TTR",
	"TTS",
	"TBI",
	"TBR",	
	"TBS",
	"Per",
	"VarMov",
	'nTim',
	"PerF",
	"VarF",
	"FFil-raw",
	"FBody-raw",
	'FTip-raw',
	'FFil',
	"FBody",
	"FTip"
)

myquant <- list(
	tidy_max.lengths,
	tidy_med.rate.extens,
	tidy_med.rate.retr,
	tidy_straightness.at.max.over5,
	tidy_med.fdcbm.invas,
	tidy_med.fdcbm.retr,
	tidy_all.time.ext,
	tidy_all.time.retr,
	tidy_all.time.stall,
	tidy_all.time.base.inv,
	tidy_all.time.base.retr,	
	tidy_all.time.base.stable,
	tidy_acf.fdctm.roots,
	tidy_var.all.move,
	tidy_n.timepoints,
	tidy_acf.tip.f.roots,
	tidy_var.tip.f,
	tidy_mean.f.proj.raw,
	tidy_mean.f.body.raw,
	tidy_mean.f.tip.raw,
	tidy_mean.f.proj,
	tidy_mean.f.body,
	tidy_mean.f.tip
)

myquant.multi.p <- unlist(lapply(myquant, function(x) wilcox.test(x[, 1] ~ Category, x)$p.value))

adjusted.p.values <- data.frame(
		"var" = myquantnames, 
		"Mann-Whitney" = myquant.multi.p, 
		"adj Bonf" = p.adjust(myquant.multi.p, method = "bonferroni"),
		"adj Holm" = p.adjust(myquant.multi.p, method = "holm")
		)
print(adjusted.p.values)
write.csv(adjusted.p.values, "Adjusted-p.csv")

setwd(metalist[[1]]$Loc)
save.image("LastWorkspace_CCF_Subclusters.Rdata")

#--------------------------------------------------------------------------------------- 
# Plot "phenotype signature" as a colour-coded heatmap of z-score / effect size:

# 1. Generate z-scores:

FindZScores <- function(x) {
	x1 <- subset(x, Category == levels(x$Category)[1]) [, 1]
	x2 <- subset(x, Category == levels(x$Category)[2]) [, 1]
	mean.x1 <- mean(x1, na.rm = TRUE)
	mean.x2 <- mean(x2, na.rm = TRUE)	
	sd.x1 <- sd(x1, na.rm = TRUE)
	sd.x2 <- sd(x2, na.rm = TRUE)
	z1 <- (mean.x2 - mean.x1) / sd.x1
	z2 <- (mean.x1 - mean.x2) / sd.x2
	return(c(z1, z2))
}

z.scores <- lapply(myquant, FindZScores)
names(z.scores) <- names(myquant)

z.matrix <- matrix(NA, nrow = 2, ncol = length(myquant))
z.matrix[1, ] <- unlist(transpose(z.scores)[[1]])
z.matrix[2, ] <- unlist(transpose(z.scores)[[2]])
colnames(z.matrix) <- myquant_abbrev

# 2. Generate Cliff delta:

library(effsize)

delta.scores <- lapply(myquant, function(x) cliff.delta(x[,1] ~ Category, data = x)$estimate)
names(delta.scores) <- myquant_abbrev
delta.matrix <- matrix(NA, nrow = 2, ncol = length(myquant))
delta.matrix[1, ] <- unlist(transpose(delta.scores)[[1]])
delta.matrix[2, ] <- unlist(transpose(delta.scores)[[1]])
colnames(delta.matrix) <- myquant_abbrev


# 3. Create signature heatmaps:

scale.edges = c(-1, 1)  # -1, 1 for Cliff's delta
scale.breaks <- seq(scale.edges[1], scale.edges[2], length.out = length(curr.pal)+1)

dev.new(width =7, height = 3.5)
heatmap.2(delta.matrix[, 1:13], 
	# Switch off reordering and dendrograms:
      Colv = FALSE, Rowv = FALSE, dendrogram = "none", 
      breaks = scale.breaks,
      symkey = F,
    # Switch off other bells and whistles:
      trace = "none",
    # Color:
      col = curr.pal,
    # Separation:
      colsep = c(1:23),  #rowsep = c(1:23),
      sepcol = "white", sepwidth = c(0.0, 0.02),
    # Legend: 
      keysize = 2, density.info = "none", key.title = "",
      key.xlab = "Effect size",
    # Labels:
      labRow = "", # dataset.names[c(2,1)], 
      cexRow = 1, cexCol = 1,
      srtCol = 45
    # Layout:
    # Note within cells:
    # cellnote = rep(as.character(1:23), 2)
)

scale.edges = c(-3, 3)  # -3, 3 for z scores
scale.breaks <- seq(scale.edges[1], scale.edges[2], length.out = length(curr.pal)+1)

dev.new(width =7, height = 3.5)
heatmap.2(z.matrix[c(2,1), 1:13], 
	# Switch off reordering and dendrograms:
      Colv = FALSE, Rowv = FALSE, dendrogram = "none", 
      breaks = scale.breaks,
      symkey = F,
    # Switch off other bells and whistles:
      trace = "none",
    # Color:
      col = curr.pal,
    # Separation:
      colsep = c(1:23),  #rowsep = c(1:23),
      sepcol = "white", sepwidth = c(0.0, 0.02),
    # Legend: 
      keysize = 2, density.info = "none", key.title = "",
      key.xlab = "z-score",
    # Labels:
      labRow = "", # dataset.names[c(2,1)], 
      cexRow = 1, cexCol = 1,
      srtCol = 45
    # Layout:
    # Note within cells:
    # cellnote = rep(as.character(1:23), 2)
)

# Plot P-values for Mann-Whitney tests (adj for multiple comparisons)

dev.new(width = 7, height = 1.5)
adj.p <- adjusted.p.values$adj.Holm; names(adj.p) <- unlist(myquant_abbrev)
par(mar = c(3,4,1,1) + 0.1)

# barplot -log p value: 
barplot(-log(adj.p[1:13], base = 10), ylim = c(0, 3), ylab = "-log(p-value)", 
	cex.names = 0.8, axes = FALSE); 
	par(yaxp = c(0, 3, 3)); 
	axis(2)
	
# barplot: actual p value on a -log scale
barplot(adj.p[1:13], ylim = c(0, 3), ylab = "p-value", 
	cex.names = 0.8, ylog = TRUE)

help(par)


abline(h = -log(0.05, base = 10), lty = 3)

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
## PLOT PHENOTYPE SUMMARY BY USING CODE FROM FILOPODYAN MODULE 3:

# Loc.save = Loc
library(RColorBrewer)
curr.pal = brewer.pal(11, "RdBu")
# display.brewer.all()

scale.edges = c(-1, 1)
scale.breaks <- seq(scale.edges[1], scale.edges[2], length.out = length(curr.pal)+1)

reference.dataset = "nonTCS"
dataset.other = "TCS"

# change myquant to 13 basic parameters:

myquant <- list(
	tidy_max.lengths,
	tidy_straightness.at.max.over5,
	tidy_med.rate.extens,
	tidy_med.rate.retr,
	tidy_med.fdcbm.invas,
	tidy_med.fdcbm.retr,
	tidy_all.time.ext,
	tidy_all.time.retr,
	tidy_all.time.stall,
	tidy_all.time.base.inv,
	tidy_all.time.base.retr,	
	tidy_all.time.base.stable,
	tidy_acf.fdctm.roots)
myquantF <- list(		
	tidy_var.tip.f,
	tidy_var.all.move,
	tidy_acf.tip.f.roots,
	tidy_mean.f.proj.raw,
	tidy_mean.f.body.raw,
	tidy_mean.f.tip.raw,
	tidy_mean.f.proj,
	tidy_mean.f.body,
	tidy_mean.f.tip,
	tidy_mean.f.proj.nor,
	tidy_mean.f.tip.nor
)
myquantnames <- c(
	"max.lengths",
	"straightness.at.max.over5",
	"med.rate.extens",
	"med.rate.retr",
	"med.fdcbm.invas",
	"med.fdcbm.retr",
	"all.time.ext",
	"all.time.retr",
	"all.time.stall",
	"all.time.base.inv",
	"all.time.base.retr",	
	"all.time.base.stable",
	"acf.fdctm.roots"
	# ,"var.all.move",
	# 'n.timepoints',
	# "acf.tip.f.roots",
	# "var.tip.f",
	# "mean.f.proj.raw",
	# "mean.f.body.raw",
	# 'mean.f.tip.raw',
	# 'mean.f.proj',
	# "mean.f.body",
	# "mean.f.tip"
)
myquantFnames <- list(		
	"var.tip.f",
	"var.all.move",
	"acf.tip.f.roots",
	"mean.f.proj.raw",
	"mean.f.body.raw",
	"mean.f.tip.raw",
	"mean.f.proj",
	"mean.f.body",
	"mean.f.tip",
	"mean.f.proj.nor",
	"mean.f.tip.nor"
)
myquant_fullnames <- c("Max length", 
			"Straightness", 
			"Tip extension rate", 
			"Tip retraction rate",
			"Base invasion rate",
			"Base retraction rate",
			"Time tip extending",
			"Time tip retracting",
			"Time tip stalling",
			"Time base invading",
			"Time base retracting",
			"Time base stalling",
			"Tip persistence")
myquantF_fullnames <- list(		
	"Variance tip F",
	"Variance tip movement",
	"Autocorr tip F",
	"Mean F proj (raw)",
	"Mean F body (raw)",
	"Mean F tip (raw)",
	"Mean F proj (bg corr)",
	"Mean F body (bg corr)",
	"Mean F tip (bg corr)",
	"Mean F proj (bg corr nor)",
	"Mean F tip (bg corr nor)"
)

names(myquant) <- myquantnames
names(myquantF) <- myquantFnames

# Check all are there:
lapply(myquant, function(x) dim(x))
lapply(myquantF, function(x) dim(x))

# Check matching:
data.frame("Parameter code" = myquantnames, "Full name" = myquant_fullnames)
data.frame("Parameter code" = unlist(myquantFnames), "Full name" = unlist(myquantF_fullnames))

# calculate necessary stats that feed into pirateplot (normalised; Cliff's delta; )

#-------------------------------------------------------------------------------
# Compute Cliff's delta:

library(effsize)

stopifnot(exists("reference.dataset"))
stopifnot(exists("dataset.other"))

delta.scores <- lapply(myquant, function(x) cliff.delta(
	x[,1] ~ factor(x$Category, levels = c(dataset.other, reference.dataset)), 
	data = x)$estimate)
delta.scores 

delta.matrix <- matrix(NA, nrow = 2, ncol = length(myquant))
	delta.matrix[1, ] <- unlist(transpose(delta.scores)[[1]])
	delta.matrix[2, ] <- unlist(transpose(delta.scores)[[1]])
	colnames(delta.matrix) <- names(myquant)


# Adjusted P-values: 

for(i in 1:length(myquant)) {
	stopifnot(length(myquant) > 0)
	if(i == 1) {
			myquant.multi.mw.test <- list() 
			myquant.multi.p <- list() 
			}
	z <- myquant[[i]]
	myquant.multi.mw.test[[i]] <- wilcox.test(z[, 1]~ Category, data = z)
	myquant.multi.p[[i]] <- wilcox.test(z[, 1] ~ Category, data = z)$p.value
}
rm(z)

adjusted.p.values <- data.frame(
		"var" = myquantnames, 
		"Mann-Whitney" = unlist(myquant.multi.p), 
		"adj Bonf" = p.adjust(unlist(myquant.multi.p), method = "bonferroni"),
		"adj Holm" = p.adjust(unlist(myquant.multi.p), method = "holm")
		)

print(adjusted.p.values)
setwd(Loc.save); write.csv(adjusted.p.values, "Adjusted-p.csv")

# 1. Normalise all data:

ScaleToRef <- function(x, reference.dataset) {
	
	ref.subset <- subset(x, subset = (Category == reference.dataset))

	ref.mean <- mean(ref.subset[,1], na.rm = TRUE)
	ref.sd <- sd(ref.subset[,1], na.rm = TRUE)

	x$"Scaled" <- (x[,1] - ref.mean) / ref.sd
	x
}

ScaleToRef_MedIQR <- function(x, reference.dataset) {
	
	ref.subset <- subset(x, subset = (Category == reference.dataset))

	ref.median <- median(ref.subset[,1], na.rm = TRUE)
	ref.iqr <- IQR(ref.subset[,1], na.rm = TRUE) 

	x$"Scaled" <- (x[,1] - ref.median) / ref.iqr
	x
}

myquant.scaled <- lapply(myquant, function(x) ScaleToRef(x, reference.dataset))
myquant.scaled_2 <- lapply(myquant, function(x) ScaleToRef_MedIQR(x, reference.dataset))

myquant.ref <- lapply(myquant.scaled, function(x) subset(x, subset = (x$Category == reference.dataset)))
myquant.other <- lapply(myquant.scaled, function(x) subset(x, subset = (x$Category != reference.dataset)))

myquant.ref2 <- lapply(myquant.scaled_2, function(x) subset(x, subset = (x$Category == reference.dataset)))
myquant.other2 <- lapply(myquant.scaled_2, function(x) subset(x, subset = (x$Category != reference.dataset)))

CombineAllParameters <- function(myquant.list, column) {
	
	# LIST -> DATAFRAME (column per parameter)
	# Takes list (such as myquant.ref or myquant.other) and combines all lists dataframes into dataframe columns, 
	# with its column title specified by names(list)
	
	z <- data.frame(matrix(NA, ncol = length(myquant.list), nrow = nrow(myquant.list[[1]])))
	
	for (i in 1:length(myquant.list)) {
		
		cur.col <- myquant.list[[i]][column]
		z[, i] <- cur.col
		
	}
	colnames(z) <- names(myquant.list)
	return(z)
}


# plot pirateplots for 13 basic parameters; save tables and p values etc.

# CUSTOMISED PLOT OF DATA:

PiratePlot <- function(formula, data, method = c("mean.SD", "median.IQR"), 
	inf.b.col = "black", inf.f.col = "grey", inf.opacity = 0.5, ...) {
	
	par(las = 2)
	par(mfrow = c(2,1))
	par(xaxt = "n")
	par(yaxt = "n")
	par(yaxp = c(-2,2,5))

pirateplot(value ~ key, data = data, bty="n",
	ylim = c(-2,2),
	xlab = "", ylab = "",
	# THEME:
	theme = 2,
		
	# Configure the order: sequential as in orginal
	sortx = "sequential",
	
	# Configure appearance:
	# ... transparency of elements:
		#avg.line.o = 1,
		# Inference interval:
		inf.b.o = 1,
		inf.f.o = inf.opacity,
		#inf.f.o = inf.f.o,
		# Beans:
		bean.b.o = 0.0,
		bean.f.o = 0.00,
		# Barplot: OFF
		bar.b.o = 0,
		# Points:
		point.o = 0.25,
		# Average line:
		#avg.line.lwd = 0,
				
	# ... color of elements
	#inf.b.col = as.character(cut(unlist(delta.scores), breaks = scale.breaks, labels = curr.pal)),
	#inf.f.col = as.character(cut(unlist(delta.scores), breaks = scale.breaks, labels = curr.pal)),
	inf.f.col = inf.f.col,
	inf.b.col = inf.b.col,
	inf.lwd = 0.5,
	
	# color of gridlines
	gl.col = "transparent",
	
	# other modifications:
	...
	)
	abline(h = 0, lwd = 1, lty = 1)
}

#  ... with reference (nonTCS):

dev.new(width = 5, height = 8)
	PiratePlot(key~value, data = gather(CombineAllParameters(myquant.ref2, column = "Scaled")),
		inf.method = "iqr",
		quant = 0.5, # add median lines (50% quantile)
		quant.lwd = 3,
		quant.col = "black",
		quant.length = 0.8,
		avg.line.o = 0,
		avg.line.lwd = 0)
	rect(par("usr")[1]-1, ybottom = -0.5, par("usr")[2]+1, ytop = 0.5, border = "#00000025")
	mtext("Median", side = 2, at = 0, cex = 0.8, srt = 90, line = 1)

	dev.copy(pdf, width = dev.size()[1], height = dev.size()[2], paste0("Rplot_Pirateplot_", reference.dataset, "_Median_halfIQR.pdf")); dev.off()

#  ... with other (TCS):

dev.new(width = 5, height = 8)
	PiratePlot(key~value, data = gather(CombineAllParameters(myquant.other2, column = "Scaled")),
		inf.method = "iqr",
		quant = 0.5, # add median lines (50% quantile)
		quant.lwd = 3,
		quant.col = "black",
		quant.length = 0.8,
		avg.line.o = 0,
		avg.line.lwd = 0,
		#inf.f.o = 1,
		inf.f.col = "grey" #as.character(cut(unlist(delta.scores), breaks = 0.5*scale.breaks, labels = curr.pal))
		)
	rect(par("usr")[1]-1, ybottom = -0.5, par("usr")[2]+1, ytop = 0.5, border = "#00000025")
	mtext("Median", side = 2, at = 0, cex = 0.8, srt = 90, line = 1)

	dev.copy(pdf, width = dev.size()[1], height = dev.size()[2], paste0("Rplot_Pirateplot_", dataset.other, "_Median_halfIQR.pdf")); dev.off()
	
# ... IN COLOUR:

#curr.pal = brewer.pal(11, "RdBu")[3:9]
#scale.edges = c(-1, 1)
#scale.breaks <- seq(scale.edges[1], scale.edges[2], length.out = length(curr.pal)+1)

dev.new(width = 5, height = 8)
	PiratePlot(key~value, data = gather(CombineAllParameters(myquant.other2, column = "Scaled")),
		inf.method = "iqr",
		quant = 0.5, # add median lines (50% quantile)
		quant.lwd = 3,
		quant.col = "black",
		quant.length = 0.8,
		avg.line.o = 0,
		avg.line.lwd = 0,
		inf.opacity = 0.5,
		inf.f.col = as.character(cut(unlist(delta.scores), breaks = 0.5*scale.breaks, labels = curr.pal))
		)
	rect(par("usr")[1]-1, ybottom = -0.5, par("usr")[2]+1, ytop = 0.5, border = "#00000025")
	mtext("Median", side = 2, at = 0, cex = 0.8, srt = 90, line = 1)

	dev.copy(pdf, width = dev.size()[1], height = dev.size()[2], paste0("Rplot_Pirateplot_", dataset.other, "_Median_halfIQR_Color.pdf")); dev.off()



if(!exists("save.summary")) {save.summary = TRUE}

if(save.summary == TRUE) {


Summary <- function(x) {
  tapply(x[, 1], x[, 2], 
    function(x.subset) {
      z <- data.frame(
        "Min"    = min(x.subset, na.rm= TRUE),  
        "1st Qu" = quantile(x.subset, 0.25, na.rm= TRUE),
        "Median" = median(x.subset, na.rm= TRUE),
        "Mean"   = mean(x.subset, na.rm= TRUE),
        "3rd Qu" = quantile(x.subset, 0.75, na.rm= TRUE),
        "Max"    = max(x.subset, na.rm= TRUE),
        "SD"     = sd(x.subset, na.rm = TRUE),
        "N"       = Count(x.subset)
      )
     rownames(z) <- NULL
    return(z) }
  )
}

results.list <- lapply(myquant, Summary)
results.list.transposed <- transpose(results.list)

ConvertListToTable <- function(x) {
  z <- data.frame(matrix(NA, ncol = 8, nrow = length(x)))
  colnames(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "SD", "N")
  z[1, ] <- x[[1]]
  z[2, ] <- x[[2]]
  rownames(z) <- names(x)
  return(z)
  } 

RESULTS <- list()
for(i in 1:length(results.list)) {
  RESULTS[[i]] <- as.data.frame(ConvertListToTable(results.list[[i]]))
}
names(RESULTS) = names(results.list)

# Combine all lists within RESULTS into a single data frame:

for(i in 1:length(RESULTS)) {
  param.name = names(RESULTS)[i] 
  if(i == 1) {

    # First iteration of loop:
    z <- data.frame(matrix(NA, nrow = nrow(RESULTS[[1]]), ncol = ncol(RESULTS[[1]])))
    colnames(z) <- colnames(RESULTS[[1]])
    z <- RESULTS[[i]]
    z$Parameter <- rep(param.name, 2)
    # ... thereafter:
  } else {
    new.part <- RESULTS[[i]] 
    new.part$Parameter <- rep(param.name, 2)
    z <- rbind(z, new.part) 
  }
}
z

z$"Condition" <- rep(c(dataset.other, reference.dataset), nrow(z)/2)  # ÇAREFUL WITH ORDER
z <- z[, c("Parameter", 
          "Condition", 
          "Min.", 
          "1st Qu.", 
          "Median", 
          "Mean", 
          "3rd Qu.", 
          "Max.",
          "SD",
          "N")]  # Cosmetic reordering of columns
z
setwd(Loc.save)
write.csv(z, "Filopodia_PropertiesSummary.csv", row.names = FALSE)

#------------------------------------------------------------------------------
# Add additional statistics to the results table: 
#   - fold change
#   - P value (Mann-Whitney before adjustment)
#   - P value (with Holm adjustment)
#   - z score (for each dataset, relative to the other dataset)
#   - Cliff's delta

# fold.changes # 2
# adjusted.p.values$Mann.Whitney # 1 only
# adjusted.p.values$adj.Holm # 1 only
# z.scores # 2 # unlist(z.scores)
# cohen.scores # 1 only
# delta.scores # 1 only

# Fold change between medians for each parameter:
MedianFoldChange <- function(x) {
	ref <- which(x$Category == reference.dataset)
	other <- which(x$Category != reference.dataset)
	m.ref <- median(x[,1][ref], na.rm = TRUE)
	m.other <- median(x[,1][other], na.rm = TRUE)
 	return (c(m.other/m.ref, m.ref/m.other))
}

MeanFoldChange <- function(x) {
	ref <- which(x$Category == reference.dataset)
	other <- which(x$Category != reference.dataset)
	m.ref <- mean(x[,1][ref], na.rm = TRUE)
	m.other <- mean(x[,1][other], na.rm = TRUE)
 	return (c(m.other/m.ref, m.ref/m.other))
}

fold.changes.medians <- lapply(myquant, MedianFoldChange)
fold.changes.means <- lapply(myquant, MeanFoldChange)
fold.changes.medians
fold.changes.means

# Quality check:

z.scores <- lapply(myquant, FindZScores)
names(z.scores) <- names(myquant)

stopifnot(
  identical(length(fold.changes.medians),
			length(fold.changes.means),
            length(z.scores),
            length(adjusted.p.values$Mann.Whitney),
            length(adjusted.p.values$adj.Holm),
            length(z.scores),
            #length(cohen.scores),
            length(delta.scores)
            )
)


result.comp <- data.frame(matrix(NA, nrow = length(myquant), ncol = 0))  # Comparison between datasets

result.comp$"Fold change (median)" <- unlist(transpose(fold.changes.medians)[[1]])  # [[]]
result.comp$"Fold change (mean)" <- unlist(transpose(fold.changes.means)[[1]])
result.comp$"z-score" <- unlist(transpose(z.scores)[[1]])  # [[1]] for Other.vs.Ref
#result.comp$"Cohen's d" <- unlist(cohen.scores)
result.comp$"Cliff's delta " <- unlist(delta.scores)
result.comp$"P (Mann-Whitney)" <- adjusted.p.values$Mann.Whitney
result.comp$"P (Holm-adjusted)" <- adjusted.p.values$adj.Holm
rownames(result.comp) <- names(myquant)
result.comp

write.csv(result.comp, "Filopodia_compare.csv")

# end of 'save.summary' section
}



# REPEAT THIS PROCESSING ON THE FLUORESCENCE MEASUREMENTS:

#-------------------------------------------------------------------------------
# Compute Cliff's delta:

library(effsize)
library(purrr)

stopifnot(exists("reference.dataset"))
stopifnot(exists("dataset.other"))

delta.scoresF <- lapply(myquantF, function(x) cliff.delta(
	x[,1] ~ factor(x$Category, levels = c(dataset.other, reference.dataset)), 
	data = x)$estimate)
delta.scoresF

delta.matrixF <- matrix(NA, nrow = 2, ncol = length(myquantF))
	delta.matrixF[1, ] <- unlist(transpose(delta.scoresF)[[1]])
	delta.matrixF[2, ] <- unlist(transpose(delta.scoresF)[[1]])
	colnames(delta.matrixF) <- names(myquantF)

# Adjusted P-values: 

for(i in 1:length(myquantF)) {
	stopifnot(length(myquantF) > 0)
	if(i == 1) {
			myquant.multi.mw.testF <- list() 
			myquant.multi.pF <- list() 
			}
	z <- myquantF[[i]]
	myquant.multi.mw.testF[[i]] <- wilcox.test(z[, 1]~ Category, data = z)
	myquant.multi.pF[[i]] <- wilcox.test(z[, 1] ~ Category, data = z)$p.value
}
rm(z)

adjusted.p.valuesF <- data.frame(
		"var" = unlist(myquantFnames), 
		"Mann-Whitney" = unlist(myquant.multi.pF), 
		"adj Bonf" = p.adjust(unlist(myquant.multi.pF), method = "bonferroni"),
		"adj Holm" = p.adjust(unlist(myquant.multi.pF), method = "holm")
		)

print(adjusted.p.valuesF)
setwd(Loc.save); write.csv(adjusted.p.valuesF, "Adjusted-p_F.csv")




if(!exists("save.summary")) {save.summary = TRUE}

if(save.summary == TRUE) {


Summary <- function(x) {
  tapply(x[, 1], x[, 2], 
    function(x.subset) {
      z <- data.frame(
        "Min"    = min(x.subset, na.rm= TRUE),  
        "1st Qu" = quantile(x.subset, 0.25, na.rm= TRUE),
        "Median" = median(x.subset, na.rm= TRUE),
        "Mean"   = mean(x.subset, na.rm= TRUE),
        "3rd Qu" = quantile(x.subset, 0.75, na.rm= TRUE),
        "Max"    = max(x.subset, na.rm= TRUE),
        "SD"     = sd(x.subset, na.rm = TRUE),
        "N"       = Count(x.subset)
      )
     rownames(z) <- NULL
    return(z) }
  )
}

results.listF <- lapply(myquantF, Summary)
results.list.transposedF <- transpose(results.listF)

ConvertListToTable <- function(x) {
  z <- data.frame(matrix(NA, ncol = 8, nrow = length(x)))
  colnames(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "SD", "N")
  z[1, ] <- x[[1]]
  z[2, ] <- x[[2]]
  rownames(z) <- names(x)
  return(z)
  } 

RESULTSF <- list()
for(i in 1:length(results.listF)) {
  RESULTSF[[i]] <- as.data.frame(ConvertListToTable(results.listF[[i]]))
}
names(RESULTSF) = names(results.listF)
RESULTSF

# Combine all lists within RESULTSF into a single data frame:

for(i in 1:length(RESULTSF)) {
  param.name = names(RESULTSF)[i] 
  if(i == 1) {

    # First iteration of loop:
    z <- data.frame(matrix(NA, nrow = nrow(RESULTSF[[1]]), ncol = ncol(RESULTSF[[1]])))
    colnames(z) <- colnames(RESULTSF[[1]])
    z <- RESULTSF[[i]]
    z$Parameter <- rep(param.name, 2)
    # ... thereafter:
  } else {
    new.part <- RESULTSF[[i]] 
    new.part$Parameter <- rep(param.name, 2)
    z <- rbind(z, new.part) 
  }
}
z

z$"Condition" <- rep(c(dataset.other, reference.dataset), nrow(z)/2)  # ÇAREFUL WITH ORDER
z <- z[, c("Parameter", 
          "Condition", 
          "Min.", 
          "1st Qu.", 
          "Median", 
          "Mean", 
          "3rd Qu.", 
          "Max.",
          "SD",
          "N")]  # Cosmetic reordering of columns
z

setwd(Loc.save)
write.csv(z, "Filopodia_PropertiesSummary_F.csv", row.names = FALSE)

#------------------------------------------------------------------------------
# Add additional statistics to the results table: 
# (not fully mature yet)
#   - fold change
#   - P value (Mann-Whitney before adjustment)
#   - P value (with Holm adjustment)
#   - z score (for each dataset, relative to the other dataset)
#   - Cliff's delta


# Fold change between medians for each parameter:
MedianFoldChange <- function(x) {
	ref <- which(x$Category == reference.dataset)
	other <- which(x$Category != reference.dataset)
	m.ref <- median(x[,1][ref], na.rm = TRUE)
	m.other <- median(x[,1][other], na.rm = TRUE)
 	return (c(m.other/m.ref, m.ref/m.other))
}

MeanFoldChange <- function(x) {
	ref <- which(x$Category == reference.dataset)
	other <- which(x$Category != reference.dataset)
	m.ref <- mean(x[,1][ref], na.rm = TRUE)
	m.other <- mean(x[,1][other], na.rm = TRUE)
 	return (c(m.other/m.ref, m.ref/m.other))
}

fold.changes.mediansF <- lapply(myquantF, MedianFoldChange)
fold.changes.meansF <- lapply(myquantF, MeanFoldChange)
fold.changes.mediansF
fold.changes.meansF


# Quality check:

z.scoresF <- lapply(myquantF, FindZScores)
names(z.scoresF) <- names(myquantF)

stopifnot(
  identical(length(fold.changes.mediansF),
			length(fold.changes.meansF),
            length(z.scoresF),
            length(adjusted.p.valuesF$Mann.Whitney),
            length(adjusted.p.valuesF$adj.Holm),
            length(z.scoresF),
            #length(cohen.scores),
            length(delta.scoresF)
            )
)


result.compF <- data.frame(matrix(NA, nrow = length(myquantF), ncol = 0))  # Comparison between datasets

result.compF$"Fold change (median)" <- unlist(transpose(fold.changes.mediansF)[[1]])  # [[]]
result.compF$"Fold change (mean)" <- unlist(transpose(fold.changes.meansF)[[1]])
result.compF$"z-score" <- unlist(transpose(z.scoresF)[[1]])  # [[1]] for Other.vs.Ref
#result.comp$"Cohen's d" <- unlist(cohen.scores)
result.compF$"Cliff's delta " <- unlist(delta.scoresF)
result.compF$"P (Mann-Whitney)" <- adjusted.p.valuesF$Mann.Whitney
result.compF$"P (Holm-adjusted)" <- adjusted.p.valuesF$adj.Holm
rownames(result.compF) <- names(myquantF)
result.compF

write.csv(result.compF, "Filopodia_compare_F.csv")

# end of 'save.summary' section
}


