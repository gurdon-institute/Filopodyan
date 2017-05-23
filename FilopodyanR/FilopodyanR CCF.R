# TIP FLUORESCENCE & MOVEMENT - general CCF script

# This script is part of a suite of scripts for analysis of filopodia dynamics 
# using the Fiji plugin Filopodyan. The questions addressed here are whether the 
# accummulation of protein of interest in tips of filopodia correlates with their
# behaviour. This effect may occur either immediately (offset = 0) or with a delay 
# (offset > 0) if the protein requires time to activate other downstream effectors
# before exerting its effect on tip movement. For this reason the script uses a cross-
# correlation function to compute cross-correlation (for each filopodium) at each 
# value of the offset. It then looks at groups of filopodia that share a similar 
# relationship between fluorescence and movement (responding vs non-responding filopodia)
# using hierarchical clustering, and compares the properties of those clusters.

# Data input: requires an .Rdata file from upstream Filopodyan .R scripts 
# (load in Section 1).

# Data output:  a CCF table (ccf.tip.dctm) and its clustered heatmap; 
# 				top-correlating subcluster ('TCS') vs other filopodia ('nonTCS')

# Downstream applications:  1. Subcluster analysis (CCF, phenotype)  2. Randomisation analysis

# For more information contact Vasja Urbancic at vu203@cam.ac.uk.

 
rm(list = ls())

# ---------------------------------------------------------------------------
# 0. DEPENDENCIES:

# Required packages:

# install.packages("Hmisc", dependencies=TRUE, repos="http://cran.rstudio.com/")
# install.packages("RColorBrewer", dependencies=TRUE, repos="http://cran.rstudio.com/")
# install.packages("wavethresh", dependencies=TRUE, repos="http://cran.rstudio.com/")
library(Hmisc)
library(RColorBrewer)
library(wavethresh)

# Functions (general):

Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))  

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }

MovingAverage <- function(x, w = 5) {
		filter(x, rep(1/w, w), sides = 2)
}

# Functions (for block randomisation):

extractBlockIndex <- function(which.block, block.size, ...) {
	start <- ((which.block-1) * block.size) + 1
	end <- ((which.block) * block.size)
	c(start:end)
}

BlockReshuffle <- function(x, block.size = 12) {
	
	stopifnot(length(x) > block.size)
	
	n.blocks <- length(x) %/% block.size
	overhang <- length(x) %% block.size
			
	included <- 1:(block.size*n.blocks)
	excluded.overhang <- setdiff(seq_along(x), included) 
	
	x.in.blocks <- list()
	for(i in 1:n.blocks) {
		x.in.blocks[[i]] <- x[extractBlockIndex(i, 12)]
	}
	
	# which blocks to keep in place (full of NAs), which blocks to swap over?
	
	max.NA.per.block <- 0.25 * block.size 
	blocks.to.shuffle <- which(lapply(x.in.blocks, Count) > max.NA.per.block)
	blocks.to.keep <- which(lapply(x.in.blocks, Count) <= max.NA.per.block)	
	
	# generate permuted blocks, plus insert NA blocks into their respective positions

	#set.seed(0.1)
	new.order <- c(sample(blocks.to.shuffle))
	for (j in blocks.to.keep) {
		new.order <- append(new.order, j, after = j-1)
	}
	
	# new vector
		
	for(k in new.order) {
		
		if(exists("z") == FALSE) {z <- c()}
		
		z <- append(z, x.in.blocks[[k]])
	}
	z <- append(z, x[excluded.overhang])
	z	
}


# ---------------------------------------------------------------------------
# 1. Load data from saved workspace

# Load data:

# ENA (as metalist):
#load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01/LastWorkspace_ENA.Rdata')

# Normalised to filopodium (proj) fluorescece:
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toFilo/LastWorkspace_ENA.Rdata')

# Normalised to GC body:
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toGC/LastWorkspace_ENA.Rdata')

# Not normalised (only bg corrected):
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_NormOFF/LastWorkspace_ENA.Rdata')


# VASP (as metalist):
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01/LastWorkspace_VASP.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_NormOFF/LastWorkspace_VASP.Rdata')
load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC/LastWorkspace_VASP.Rdata')


# Check normalisation method:
metalist[[1]]$nor.tip.setting

# Check background correction method:
metalist[[1]]$bg.corr.setting

# Saving location:
metalist[[1]]$Loc <- folder.names[1]
metalist[[1]]$Loc

# ---------------------------------------------------------------------------
# 2. Extract equivalent data from within the metalist:

	all.dS 	  <- metalist[[1]]$all.dS
	dS.vector <- metalist[[1]]$dS.vector
	bb        <- metalist[[1]]$bb
	max.t     <- metalist[[1]]$max.t
	spt       <- metalist[[1]]$spt
	threshold.ext.per.t <- metalist[[1]]$threshold.ext.per.t
	threshold.retr.per.t <- metalist[[1]]$threshold.retr.per.t
	
	tip.f <- metalist[[1]]$tip.f 		
	all.move  <- metalist[[1]]$all.move
	

# Options for using FDCTM instead of raw DCTM, and smoothed tipF signal:

# If use.fdctm == TRUE?

use.fdctm = TRUE

if(use.fdctm == FALSE) {
	all.move <- metalist[[1]]$all.dctm99
}	


use.ftip = FALSE

if(use.ftip == TRUE) {
	tip.f <- apply(tip.f, 2, MovingAverage)
}

# Use difference from last timepoint, instead of actual data? (Uncomment if yes.)

# all.move <- apply(all.move, 2, diff)
# tip.f <- apply(tip.f, 2, diff)

# Difference for tip F, raw for movement:
# all.move <- all.move[2:max.t, ]
# tip.f <- apply(tip.f, 2, diff)

# Difference for movement, raw for tip F:
# all.move <- apply(all.move, 2, diff)
# tip.f <- tip.f[2:max.t, ]


# ---------------------------------------------------------------------------
# 3. Necessary data restructuring:

#  3a) - shift up the all.move table by one timepoint: 

start.row <- bb+2
stop.row  <- max.t

if (bb > 0) {
	reshuffle.vec <- c(1:bb, start.row:stop.row, bb+1)
} else if (bb == 0) {
	reshuffle.vec <- c(start.row:stop.row, bb+1)	
}

all.move <- all.move[reshuffle.vec, ]; all.move[max.t, ] <- NA

#  3b) - check if any columns have zero DCTM measurements to remove from dataset 
#      (would trip CCF calculations and heatmaps):

n.timepoints <- colSums( !is.na(all.move)); n.timepoints  
zero.lengths <- which(n.timepoints == 0); zero.lengths

if (length(zero.lengths) > 0) {
	remove.cols <- zero.lengths
	all.move  <- all.move[, -zero.lengths]	
	tip.f         <- tip.f[, -zero.lengths]	
	all.dS	 <- all.dS[, -zero.lengths]
	n.timepoints <- n.timepoints[-zero.lengths]
	rm(remove.cols)
}

short.lengths <- which(n.timepoints < 17); short.lengths

if (length(short.lengths) > 0) {
	remove.cols <- short.lengths
	all.move  <- all.move[, -short.lengths]	
	tip.f         <- tip.f[, -short.lengths]
	all.dS	 <- all.dS[, -short.lengths]
	n.timepoints <- n.timepoints[-short.lengths]
	rm(remove.cols)
}

#  ---------------------------------------------------------------------------
# Derived datasets:

# 4a) Create z scores

z.move <- scale(all.move, scale = TRUE, center = TRUE)
z.tip <- scale(tip.f, scale = TRUE, center = TRUE)


# 4b) Split all.move into all.ext, all.retr, all.stall

all.states <- cut(all.move,
    breaks = c(-Inf, threshold.retr.per.t, threshold.ext.per.t, Inf), 
    labels = c("Retr", "Stall", "Ext"))	

all.ext <- all.move; all.ext[which(all.states != "Ext")] <- NA
all.retr <- all.move; all.retr[which(all.states != "Retr")] <- NA
all.stall <- all.move; all.stall[which(all.states != "Stall")] <- NA

# illustrate how this works:
data.frame("Movement" = all.move[, 2], 
           "Ext" = all.ext[, 2],
           "Stall" = all.stall[, 2],
           "Retr" = all.retr[, 2])[22:121, ]


# ---------------------------------------------------------------------------
# 5. Explore correlations (over whole population) with XY scatterplots

dev.new(width = 7, height = 3.5)
par(mfrow = c(1,2))
par(mar = c(4,5,2,1)+0.1)

matplot(tip.f, all.move,
	pch = 16, cex = 0.8,
	col = "#41B6C420", 
	xlab = "Tip fluorescence [a.u.]",
#	xlab = expression(Delta * "Tip Fluorescence / Projection Fluorescence [a.u.]"),
	ylab = expression("Tip Movement [" * mu * "m]"),
#	ylab = expression(Delta * "Tip Movement [" * mu * "m]"),
	main = ""
)
	abline(h = 0, lty = 2, col = "grey")
#	abline(v = 1, lty = 2, col = "grey")
	abline(v = 0, lty = 2, col = "grey")

rho <- cor.test(unlist(as.data.frame(tip.f)), unlist(as.data.frame(all.move)), na.action = "na.exclude")$estimate

legend("bottomright", legend = paste("Pearson Rho =", signif(rho, 2)), cex= 0.8, bty = "n")


# As above, with z-scores:

# dev.new()
matplot(z.tip, z.move,
	pch = 16, cex = 0.8,
	col = "#41B6C420", 
	xlab = "Tip fluorescence [z-score]",
#	xlab = expression(Delta * "Tip Fluorescence / Projection Fluorescence [a.u.]"),
	ylab = expression("Tip Movement [z-score]"),
#	ylab = expression(Delta * "Tip Movement [" * mu * "m]"),
	main = ""
)
	abline(h = 0, lty = 2, col = "grey")
#	abline(v = 1, lty = 2, col = "grey")
	abline(v = 0, lty = 2, col = "grey")

rho.z <- cor.test(unlist(as.data.frame(z.tip)), unlist(as.data.frame(z.move)), na.action = "na.exclude")$estimate

legend("bottomright", legend = paste("Pearson Rho =", signif(rho.z, 2)), cex= 0.8, bty = "n")

range(tip.f, na.rm = TRUE)

dev.new(width = 3.5, height = 3.5)
hist(unlist(tip.f), col = "grey", border = "white", main = "", xlab = "TipF")


# ---------------------------------------------------------------------------
# 6. Calculate CCFs from tip F and tip movement tables

maxlag = 20
lag.range <- -maxlag:maxlag
lag.in.s  <- lag.range * spt

ccf.tip.dctm <- data.frame(matrix(NA, ncol = ncol(all.move), nrow = 2*maxlag + 1))
all.filo  <- seq_along(colnames(all.move))


for (i in all.filo) {
	ccf.i  <- ccf(tip.f[, i], all.move[, i], lag.max = 20, na.action = na.pass, plot = FALSE) 
	ccf.tip.dctm[, i] <- ccf.i
	rm(ccf.i, ccf.z.i)
}

colnames(ccf.tip.dctm) <- colnames(all.move)
row.names(ccf.tip.dctm)  <- lag.in.s


# The lag k value returned by ccf(x, y) estimates the correlation between x[t+k] and y[t].
# i.e. lag k for ccf(tip, move) estimates correlation between tip.f[t+k] and move[t]
# i.e. lag +2 means correlation between tip.f[t+2] and move[t] --> tip.f lagging behind movement
# i.e. lag -2 means correlation between tip.f[t-2] and move[t] --> tip.f leading ahead of movement

# ---------------------------------------------------------------------------
# 7. Compute and plot weighted CCFs  (optional pre-clustering)

#  7a) - Compute weighted CCF metrics:

weights.vec      <- n.timepoints
mean.ccf    <- apply(ccf.tip.dctm, 1, mean, na.rm = TRUE)
w.mean.ccf  <- apply(ccf.tip.dctm, 1, weighted.mean, w = weights.vec, na.rm = TRUE)
w.var.ccf <- apply(ccf.tip.dctm, 1, wtd.var, weights = weights.vec); w.var.ccf
w.sd.ccf  <- sqrt(w.var.ccf); w.sd.ccf
counts.ccf  <- apply(ccf.tip.dctm, 1, Count); counts.ccf
w.ci.ccf  <- 1.96 * w.sd.ccf / sqrt(counts.ccf); w.ci.ccf
ci.ccf = apply(ccf.tip.dctm, 1, CI)

filo.ID.weights <- data.frame("Filo ID" = names(ccf.tip.dctm), "Timepoints" = weights.vec); filo.ID.weights


#  7b) - Plot weighted vs unweighted

dev.new()
matplot(lag.in.s, ccf.tip.dctm, type = "l",
	main = "Cross-correlation of tip fluorescence and movement",
	ylab = "CCF (Tip Fluorescence & DCTM (99%, smoothed))",
	xlab = "Lag [s]",
	col = rgb(0,0,0,0.12),
	lty = 1
	)
abline(v = 0, col = "black", lty = 3)
abline(h = 0, col = "black", lwd = 1)
lines (lag.in.s, w.mean.ccf, 								# RED: new mean (weighted)
	col = 'red',
	lwd = 4)
ci1 = w.mean.ccf + w.ci.ccf
ci2	= w.mean.ccf - w.ci.ccf
DrawErrorAsPolygon(lag.in.s, ci1, ci2, col = rgb(1,0,0,0.2))	
lines (lag.in.s, mean.ccf, 									# BLUE: old mean (unweighted)
	col = 'blue',
	lwd = 4)
ci1 = mean.ccf + ci.ccf
ci2	= mean.ccf - ci.ccf
DrawErrorAsPolygon(lag.in.s, ci1, ci2, col = rgb(0,0,1,0.2))	

text(-40, -0.5, "Mean and 95% CI", pos = 4, col = "blue")
text(-40, -0.6, "Weighted Mean and Weighted 95% CI", col = "red", pos = 4)


#  7c) -  Lines coloured according to weighting:
#         (??colorRampPalette)

weights.vec
weights.vec2 = weights.vec / max(weights.vec)
palette.Wh.Bu <- colorRampPalette(c("white", "midnightblue"))
palette.Wh.Cor <- colorRampPalette(c("white", "#F37370")) # coral colour palette for second dataset
palette.Wh.Bu(20)
palette.Wh.Cor(20)

# Vector according to which to assign colours:
weights.vec
weights.vec2
weight.interval <- as.numeric(cut(weights.vec, breaks = 10))
w.cols <- palette.Wh.Bu(60)[weight.interval] 
w.cols.Coral <- palette.Wh.Cor(60)[weight.interval] 

data.frame(weights.vec, weights.vec2, weight.interval, w.cols )

dev.new()
	matplot(lag.in.s, ccf.tip.dctm, type = "l",
		col = w.cols,
		lty = 1,
		main = "Cross-correlation of tip fluorescence and movement",
		ylab = "CCF (Tip Fluorescence & Movement)",
		xlab = "Lag [s]"
	)
	abline(v = 0, col = "black", lty = 3)
	abline(h = 0, col = "black", lwd = 1)
	lines(lag.in.s, w.mean.ccf, 								# MIDNIGHTBLUE: new mean (weighted)
		col = 'midnightblue',
		lwd = 4)
ci1 = w.mean.ccf + w.ci.ccf
ci2	= w.mean.ccf - w.ci.ccf
palette.Wh.Bu(20)[20]
palette.Wh.Bu(20)[20]

text(-40, -0.6, "Weighted Mean + 95% CI", col = 'midnightblue', pos = 4)

DrawErrorAsPolygon(lag.in.s, ci1, ci2, col = "#19197020")


# ---------------------------------------------------------------------------
# 8. Heatmaps and clustering
#    	display.brewer.all()
#    	??heatmap

# This function creates n clusters from input table (based on euclid 
# distance *in rows 18:24* (corresponding here to lags from -6 to +6)) 

GoCluster <- function(x, n.clusters) {
	map.input <- t(x)
	distance <- dist(map.input[, 18:24], method = "euclidean")
	cluster <- hclust(distance, method = "complete")
	cutree(cluster, k = n.clusters)
}

# This function extracts indices for filo of n-th subcluster within the cluster:

nthSubcluster <- function(x, n.clusters, nth) {	
	which(GoCluster(x, n.clusters = n.clusters) == nth)
}

nthSubclusterOthers <- function(x, n.clusters, nth) {
  which(GoCluster(x, n.clusters = n.clusters) != nth)
}

# nthSubcluster(ccf.tip.dctm, n.clusters = 2, nth = 1)
# lapply(all.ccf.tables, function(x) nthSubcluster(x, 2, 1))


# ---------
# HEATMAPS:

# extract values for the heatmap scale min and max:


myHeatmap <- function(x) {
	map.input = t(x)
	distance <- dist(map.input[, 18:24], method = "euclidean")
	cluster <- hclust(distance, method = "complete")
	heatmap(map.input, Rowv = as.dendrogram(cluster), Colv = NA, xlab = "Lag", col = brewer.pal(12, "YlGnBu"), 	scale = "none")	
}

dev.new()
myHeatmap(ccf.tip.dctm[, which(colSums(!is.na(ccf.tip.dctm)) != 0)])

# table(GoCluster(ccf.tip.dctm, 5))
# table(GoCluster(ccf.tip.dctm, 7))
# table(GoCluster(ccf.tip.dctm, 8))
# table(GoCluster(ccf.tip.dctm, 9))

Edges <- function(x) c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
printEdges <- function(x) print(c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)))

heatmap.edges <- Edges(ccf.tip.dctm); 
heatmap.edges

setwd(folder.names[1]); getwd()
save.image("LastWorkspace_CCFs.Rdata")
# graphics.off()




