# This script imports and prepares data for analysis with the Filopodyan_Correlations.R 
# script. Analogous to Filopodyan Modules 1 and 2, with an additional functionality to 
# calculate properties of filopodia after a brief period of initial existence (e.g. after
# the first ten timepoints), for the purpose of correlating parameters such as initial tip 
# movement with subsequent properties of filopodia later in their life.

#-------------------------------------------------------------------------------
# Clean current workspace. WARNING: deletes everything in current workspace

rm(list = ls())
ls()

#-------------------------------------------------------------------------------
# Setting Working Directory locations:

# DATA:

# Where is your data located?

folder.names <- c(
    folder.name1 = c("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-02-16_TOTAL-CTRL_Correlations/TOTAL-ANALYSIS_RENYI2-6_ED4_v20170201")
    )   									# <---- Set location of data tables here
n.fold <- length(folder.names)
cat("Number of folders to analyse :", n.fold)
folder.names[]

dataset.names <- c(
	dataset1 = "CTRL_s9-s13_Renyi2-6"      # <---- Insert dataset names here.
	)

setwd(folder.names[1])
getwd()

Loc.save <- ""  # <----- SET SAVING LOCATION

# Module 1 elements:

spt <- 2  # seconds per timepoint
pxw <- 0.065 # pixel width in microns
bb <- 0 # base back-projections 


#-------------------------------------------------------------------------------
# Read all data tables of "[...] filopodia.txt"

Loc <- getwd()
all.names <- list.files(as.character(Loc), pattern = "*Filopodia.txt")
all.names2 <- list.files(as.character(Loc), pattern = "*Coordinates.txt")
all.names
all.names2

# Measure max number of rows in any table, important for binding tables later:

n.tables <- length(all.names)
n.tables
rows <- vector(mode = "numeric", length = 0)

for (i in seq_along(all.names)) {
	rows <- append(rows, nrow(read.table(all.names[i], sep = "\t", skip = 1)))
}

max.t <- max(unlist(rows))
max.t  # Max number of rows per table.
rm(rows) 


# --- --- --- OPTIONAL USER INPUT: --- --- ---
# Insert a specifier for import (e.g. "CTRL" or "s09") in order to select only a 
# subset of tables for processing.
# TODO(vasja): input this as a string on top of script (in "User input").

extract <- grep(pattern = c(""), x = all.names)  
#                            ^ Insert specifier inside "" (e.g. "CTRL"). 
extract
# --- --- ---  end of section  --- --- --- --- 


tip.table <- data.frame(matrix(NA, nrow = max.t, ncol = 0))

for (i in extract) {
	all.names[i]
		vec      	   = readLines(all.names[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))
		tab      	   = read.table(all.names[i], sep = "\t", skip = 1)
		colnames(tab)  = header
		top.up.rows    = max.t - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab), 
						nrow = top.up.rows))
		colnames(top.up.table) = header
		tip.table.i    = rbind(tab, top.up.table)
		tip.table	   = cbind(tip.table, tip.table.i)	
		rm(vec, header, tab, top.up.rows, top.up.table, tip.table.i)			
}
tip.table <- tip.table[names(tip.table) != "dT"]
tip.table[1:25, 1:9]  # Print top of the table for example filopodium 1.


coord.table <- data.frame(matrix(NA, nrow = max.t, ncol = 0))  # CAREFUL!! Problems with current export format. See Module 1.1 (Import Coordinates)

for (i in extract) {
	all.names2[i]
		vec      	   = readLines(all.names2[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))
		tab      	   = read.table(all.names2[i], sep = "\t", skip = 1)
		colnames(tab)  = header
		top.up.rows    = max.t - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab), 
						nrow = top.up.rows))
		colnames(top.up.table) = header
	#	coord.table.i  = rbind(tab, top.up.table)
		coord.table.i  = rbind(top.up.table, tab) # This should fix straightness import
		coord.table	   = cbind(coord.table, coord.table.i)	
		rm(vec, header, tab, top.up.rows, top.up.table, coord.table.i)			
}
coord.table <- coord.table[, - (which((names(coord.table) == " ")))]
coord.table[1:40, 1:12]



#-------------------------------------------------------------------------------

# Creating tables by metric (Length, dL, DCTM, dB, Tip Fl., Base Fl.),
# as well as corresponding timepoints of the original timelapse (T) and 
# the time from the first appearance of the filopodium (dT).
# Each table combines data for that metric for all filopodia, from all tables.

names(tip.table[1:9])

 all.T      <- tip.table[, grep(pattern = c("^T "), x = names(tip.table), 
  value = FALSE)] 

all.dT     <- tip.table[, grep(pattern = c("dT"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dS     <- all.dT * spt  # Seconds per timepoint defined on top of script.

dS.vector  <- apply(all.dS, 1, mean, na.rm=TRUE)

all.base   <- tip.table[, grep(pattern = c("Base Mean"), x = names(tip.table),
  value = FALSE, fixed = TRUE)]
  
all.body   <- tip.table[, grep(pattern = c("Body Mean"), x = names(tip.table),
  value = FALSE, fixed = TRUE)]
  
all.base.nor <- all.base / all.body  

all.tip    <- tip.table[, grep(pattern = c("Tip Mean"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)] 

all.th.tip <- tip.table[, grep(pattern = c("Tip Th Mean"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]    # Added 17.8.

all.proj   <- tip.table[, grep(pattern = c("Proj"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]

all.tip.nor1 <- all.th.tip / all.proj  # normalise to projection intensity
all.tip.nor2 <- all.th.tip / all.body  # normalise to GC body intensity

all.length <- tip.table[, grep(pattern = c("Length"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dL     <- tip.table[, grep(pattern = c("dL"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dctm   <- tip.table[, grep(pattern = c("DCTM"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dcbm  <- tip.table[, grep(pattern = c("DCBM"), x = names(tip.table),  # Added 17.8.
  value = FALSE, fixed = TRUE)]
  
all.dB     <- all.dctm - all.dL

ls()


#-------------------------------------------------------------------------------
# BounderR Module 1B  -  DATA CLEAN-UP 
#-------------------------------------------------------------------------------

# The 99% filter for DCTM and dB values:
# Remove data outside the 0.5-99.5th percentiles for DCTM and dB. These are 
# likely to be erroneous due to large displacement of the tip when experiencing 
# driftin in and out of focus, or the length being fully/partially reconstructed. 
# Base displacement of very large magnitude occur when very small widening of the 
# protrusion near the base moves the asigned base position outwards (due to 
# discrete cutoff in base assignment during the segmentation process, as specified
# with ROI erosion-dilation [ED] cycles). 

# The smoothing filter for DCTM and dB values: 
# Applies a moving average smoothing to DCTM and dB tables, by default with a step 
# size of 5.


#-------------------------------------------------------------------------------
# Create DCTM and dB value tables with a 99 percent distribution filter 
# (0.5-99.5th percentiles)

dctm.mincut <- quantile(unlist(all.dctm), probs = c(0.005, 0.995), na.rm = TRUE)[1] 
dctm.maxcut <- quantile(unlist(all.dctm), probs = c(0.005, 0.995), na.rm = TRUE)[2]

dB.mincut <- quantile(unlist(all.dB), probs = c(0.005, 0.995), na.rm = TRUE)[1] 
dB.maxcut <- quantile(unlist(all.dB), probs = c(0.005, 0.995), na.rm = TRUE)[2] 

dcbm.mincut <- quantile(unlist(all.dcbm), probs = c(0.005, 0.995), na.rm = TRUE)[1] 
dcbm.maxcut <- quantile(unlist(all.dcbm), probs = c(0.005, 0.995), na.rm = TRUE)[2]


RemoveOutliers <- function(x, mincut, maxcut) {
	y <- x
	y[x < mincut] <- NA
	y[x > maxcut] <- NA
	y
}

all.dctm99 <- RemoveOutliers(all.dctm, dctm.mincut, dctm.maxcut)
all.dB99   <- RemoveOutliers(all.dB, dB.mincut, dB.maxcut)
all.dcbm99 <- RemoveOutliers(all.dcbm, dcbm.mincut, dcbm.maxcut)


#-------------------------------------------------------------------------------
# Create DCTM and dB value tables smoothed with a travelling mean filter 

MovingAverage <- function(x, w = 5) {
		filter(x, rep(1/w, w), sides = 2)
}

sm.dctm99 <- apply(all.dctm99, 2, MovingAverage)  # Mean-smoothed DCTM values after 99% filter
sm.dB99   <- apply(all.dB99, 2, MovingAverage)   # Mean-smoothed dB values after 99% filter 
sm.dcbm99 <- apply(all.dcbm99, 2, MovingAverage)   # Mean-smoothed DCBM values after 99% filter 

all.fdctm <- sm.dctm99  # 'f' stands for 'filtered' (i.e. 'filtered DCTM')
all.fdB   <- sm.dB99 
all.fdcbm <- sm.dcbm99

rm(sm.dB99, sm.dctm99, sm.dcbm99)
ls()


#-------------------------------------------------------------------------------
# EXTRACTING DESCRIPTIVE METRICS per filopodium
#-------------------------------------------------------------------------------
# And look at cross-correlations.

# - Max Length							:   max.lengths
# - Median tip extension rate (DCTM +ve)  :	med.rate.dctm.plus 
# -    (as above, FDCTM > 0.065)	      : med.rate.extens
# - Median tip retraction rate (DCTM -ve) :	med.rate.dctm.minus
# -    (as above, FDCTM < -0.065)		  : med.rate.retract	
# - Median base invasion (dB +ve)		:	med.dB.invas
# - Median base retraction (dB -ve)		: 	med.dB.retract
# - Consistency of growth (the ACF root) 
#										:	acf.dctm.roots
# - Initial DCTM (new filo only)		:	dctm99.new.early.mean / .med
# - Initial dB (new filo)				:	dB99.new.early.mean / .med
# - Straightness						: 	straightness.at.max; waviness (1 - straightness)
 

#-------------------------------------------------------------------------------

# Optional: Suppress plotting

draw.plots = FALSE

Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }



#-------------------------------------------------------------------------------
# Max length

max.lengths <- apply(all.length, 2, max, na.rm = TRUE)

if(draw.plots == TRUE) {

	dev.new()
		hist(max.lengths, 
			col = 'cadetblue', border = 'white',
			main = "Histogram of Max Lengths",
			xlab = expression("Max Length [" * mu * "m]") )
}

#-------------------------------------------------------------------------------
# Tip extension and rectraction rates (median per filopodium)

# Threshold:

pxw <- pxw  # USER-DEFINED AT TOP OF SCRIPT

threshold.ext.per.t = pxw
threshold.retr.per.t = -pxw


MedianOfPositive <- function(x) {   
	median(subset(x, x > 0), na.rm = TRUE)
}
MedianOfNegative <- function(x) {
	median(subset(x, x < 0), na.rm = TRUE)
}

MedianOfExtending <- function(x) {
	median(subset(x, x > threshold.ext.per.t), na.rm = TRUE)
}

MedianOfRetracting <- function(x) {
	median(subset(x, x < threshold.retr.per.t), na.rm = TRUE)
}


med.rate.dctm.plus <- apply(all.dctm99, 2, MedianOfPositive)
med.rate.dctm.minus <- apply(all.dctm99, 2, MedianOfNegative) 

med.rate.extens <- apply(all.fdctm, 2, MedianOfExtending)  # COMES FROM FDCTM!
med.rate.retract <- apply(all.fdctm, 2, MedianOfRetracting) # COMES FROM FDCTM!

# defining post-10 timepoints:
post10 <- (bb+11):max.t

med.rate.extens.post10 <- apply(all.fdctm[post10, ], 2, MedianOfExtending)  med.rate.retract.post10 <- apply(all.fdctm[post10, ], 2, MedianOfRetracting) 


if( draw.plots == TRUE) {
dev.new()
	hist(med.rate.extens,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median invasion rates per base",
		xlab = expression("Median dB (> 0) [" * mu * "m / 2 s]") )
dev.new()		
	hist(med.rate.retract,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median retraction rates per base",
		xlab = expression("Median dB (< 0) [" * mu * "m / 2 s]") )
}

# Base invasion and rectraction rates (median per filopodium)

med.dB.invas <- apply(all.dB99, 2, MedianOfPositive) # redundant with dcbm below
med.dB.retract <- apply(all.dB99, 2, MedianOfNegative)  # redundant with dcbm below
med.fdB.invas <- apply(all.fdB, 2, MedianOfPositive) # redundant with dcbm below
med.fdB.retract <- apply(all.fdB, 2, MedianOfNegative)  # redundant with dcbm below

med.fdB.invas.post10 <- apply(all.fdB[post10, ], 2, MedianOfPositive) # redundant with dcbm below
med.fdB.retract.post10 <- apply(all.fdB[post10, ], 2, MedianOfNegative)  # redundant with dcbm below



# med.dcbm.invas <- apply(all.fdcbm, 2, MedianOfPositive)  # Comes from FDCBM! (5-step averaged data) 
# med.dcbm.retract <- apply(all.fdcbm, 2, MedianOfNegative) # (see above)

# Code change on 28.8.16: update from MedianOfPositive/MedianOfNegative 
med.fdcbm.invas <- apply(all.fdcbm, 2, MedianOfExtending)  # 
med.fdcbm.retract <- apply(all.fdcbm, 2, MedianOfRetracting) #

med.fdcbm.invas.post10 <- apply(all.fdcbm[post10, ], 2, MedianOfExtending)  # 
med.fdcbm.retract.post10 <- apply(all.fdcbm[post10, ], 2, MedianOfRetracting) #



if( draw.plots == TRUE) {
dev.new()
	hist(med.dB.invas,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median invasion rates per base",
		xlab = expression("Median dB (> 0) [" * mu * "m / 2 s]") )
dev.new()		
	hist(med.dB.retract,
		col = 'cadetblue', border = 'white',
		main = "Histogram of median retraction rates per base",
		xlab = expression("Median dB (< 0) [" * mu * "m / 2 s]") )
}


#-------------------------------------------------------------------------------
# Consistency of growth (DCTM ACF roots)

dctm <- all.dctm99

MaxLag <- 120

AcfTable <- function(x, L) {
	y <- data.frame(matrix(NA, ncol = ncol(x), nrow = L + 1))
	colnames(y) <- colnames(x)
	for (i in 1:ncol(x)) {
		acf.i <- as.vector(acf(x[, i], na.action = na.pass, lag.max = MaxLag, plot = FALSE)[[1]])
		y[, i] <- acf.i
		rm(acf.i)
		}
	y	
}
acf.dctm <- AcfTable(dctm, 120) 

# Extract first negative in column (the more strongly correlated the timepoints, the longer it takes for ACF to decay to 0; the offset of decay to 0 is taken as a measure of growth consistency ["ACF (DCTM) crosspoint")]

FirstNegative <- function(x) {
	if(sum(!is.na(x)) > 0) {				
		min (which(x <= 0), na.rm = TRUE)	# requires at least one non-NA value in vector (if >3 elements in dctm column acf is not computed, so acf is NA throughout, which returns Inf) 
	} else {
		NA									# returns NA in place of Inf
	}
}

acf.dctm.roots <- apply (acf.dctm, 2, FirstNegative)

if( draw.plots == TRUE) {
	hist(acf.dctm.roots, breaks = 25, col = 'salmon', border = 'white')
}
acf.dctm.roots 

# Repeat for fdctm:
acf.fdctm <- AcfTable(all.fdctm, 120)
acf.fdctm.roots <- apply (acf.fdctm, 2, FirstNegative)

#-------------------------------------
#-------------------------------------------------------------------------------
# Extracting new vs pre-existing filopodia: 

min.dT <- apply(all.dT, 2, min, na.rm=TRUE)
min.T <- apply(all.T, 2, min, na.rm=TRUE)

new <- which(min.T > 1); new
exist <- which(min.T == 1); exist
from.short <- which(all.length[bb+1, ] < 2)
new.from.short <- which((min.T > 1) & (all.length[bb+1, ] < 2))   # Filopodia that come into existence, and <2um at t1
non.new.from.short <- which((!min.T > 1) | (!all.length[bb+1, ] < 2))  # Filo pre-existing or appearing into focus

Count(new)
Count(exist)  # Added to code on 26 August
Count(new) + Count(exist)
Count(new.from.short)
Count(non.new.from.short)
Count(new.from.short) + Count(non.new.from.short) # sanity check: are all filopodia there? 

#-------------------------------------------------------------------------------
# Initial DCTM and dB [over nt (10) timepoints] for new filopodia

nt <- 10  # Number of timepoints of interest post-formation
bb  # Base backprojections (variable defined in Module 1)
early <- (bb+1):(bb+nt)  # vector of the timepoints required in this section 
early

dctm99.new.early.med <- apply(all.dctm99[early, new.from.short], 2, median, na.rm = TRUE)
dctm99.new.early.mean <- apply(all.dctm99[early, new.from.short], 2, mean, na.rm = TRUE) 

fdctm.new.early.med <- apply(all.dctm99[early, new.from.short], 2, median, na.rm = TRUE)
fdctm.new.early.mean <- apply(all.dctm99[early, new.from.short], 2, mean, na.rm = TRUE)


if( draw.plots == TRUE) {
dev.new()
	hist(dctm99.new.early.mean,
		col = 'cadetblue', border = 'white',
		main = "Histogram of initial DCTM per filopodium",
		xlab = expression("DCTM [" * mu * "m / 2 s]") )
}

dcbm99.new.early.med <- apply(all.dcbm99[early, new.from.short], 2, median, na.rm = TRUE)
dcbm99.new.early.mean <- apply(all.dcbm99[early, new.from.short], 2, mean, na.rm = TRUE)

fdcbm.new.early.med <- apply(all.fdcbm[early, new.from.short], 2, median, na.rm = TRUE)
fdcbm.new.early.mean <- apply(all.fdcbm[early, new.from.short], 2, mean, na.rm = TRUE)

fdB.new.early.med <- apply(all.fdB[early, new.from.short], 2, median, na.rm = TRUE)
fdB.new.early.mean <- apply(all.fdB[early, new.from.short], 2, mean, na.rm = TRUE)


if( draw.plots == TRUE) {
	dev.new()
		hist(dB99.new.early.mean,
			col = 'cadetblue', border = 'white',
			main = "Histogram of initial dB per filopodium",
			xlab = expression("DCTM [" * mu * "m / 2 s]") )
		dB99.new.early.mean

	dev.new()
		plot(dctm99.new.early.mean, dB99.new.early.mean, 
			ylim = c(-0.5,0.5), xlim = c(-0.5,0.5),
			pch = 16, col = 'black',
			main = "DCTM vs dB per filopodium over initial 10 timepoints",
			xlab = expression("Mean DCTM [" * mu * "m / 2 s]"),
			ylab = expression("Mean dB [" * mu * "m / 2 s]") )
		abline(h = 0, col = 'red', lty = 3)
		abline(v = 0, col = 'red', lty = 3)	
}


# as above, reformatted (so as to include NA for non-new filopodia):

med.fdctm.initial <- apply(all.fdctm[early, ], 2, median, na.rm = TRUE); med.fdctm.initial[non.new.from.short] <- NA
med.fdcbm.initial <- apply(all.fdcbm[early, ], 2, median, na.rm = TRUE); med.fdcbm.initial[non.new.from.short] <- NA
med.fdB.initial <- apply(all.fdB[early, ], 2, median, na.rm = TRUE); med.fdB.initial[non.new.from.short] <- NA


#-------------------------------------------------------------------------------
# Straightness of filopodia:

# 1. Calculate euclidean distance from Base(X,Y) to Tip (X,Y)
# 2. straightness = euclidean distance / length 

X.bases <- coord.table [, grep(pattern = c("Base X" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]
Y.bases <- coord.table [, grep(pattern = c("Base Y" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]
X.tips  <- coord.table [, grep(pattern = c("Tip X" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]
Y.tips  <- coord.table [, grep(pattern = c("Tip Y" ), x = names(coord.table), 
  value = FALSE, fixed = TRUE)]


all.euclid <- sqrt( (X.tips - X.bases)^2 + (Y.tips - Y.bases)^2 )

all.straightness <- all.euclid/all.length
all.waviness     <- 1 - all.straightness

straightness.mean <- apply(all.straightness, 2, mean, na.rm = TRUE)
waviness.mean     <- apply(all.waviness, 2, mean, na.rm = TRUE)
length.mean       <- apply(all.length, 2, mean, na.rm = TRUE)

IndexOfMax <- function(x) {
  max.x <- max(x, na.rm = TRUE)
  z <- which(x == max.x) 
  z
  if(length(z) > 1) {z <- z[1]}
  z
}
index.max.length <- apply(all.length, 2, IndexOfMax)

straightness.at.max <- c()
for(i in seq_along(colnames(all.straightness))) {
  straightness.at.max[i] <- all.straightness[index.max.length[i], i]
}

straightness.at.max

matplot(max.lengths, straightness.at.max, type = "p", pch = 16)
matplot(max.lengths, straightness.mean, type = "p", pch = 16)
abline(v = 4, col = "red")

straightness.at.max.over5 <- straightness.at.max; straightness.at.max.over5[which(max.lengths < 5)] <- NA 

matplot(all.length, all.straightness, type = "p", pch = 4, col = "#00000008",
        ylim = c(0,2), xlim = c(5,18))
    abline(h = 1, lty = 3, col = 'grey')

matplot(max.lengths, straightness.at.max.over5, type = "p", pch = 16)
# data.frame(all.length[, 4], all.straightness[, 4], all.euclid[, 4], X.bases[, 4])

if( draw.plots == TRUE) {
	dev.new()
		matplot(unlist(all.length), unlist(all.waviness), 
		main = "Waviness vs Length",
		ylab = "Waviness", xlab = expression("Length [" * mu * "m]"),
		pch = 4, cex = 0.4, col = rgb(0,0,0,0.2))
	    abline(h = 0.38, col = rgb(1,0,0,0.5), lty = 3)	
	    abline(v = 1.8, col = rgb(1,0,0,0.5), lty = 3)

	dev.new()
		matplot(length.mean, waviness.mean,
		main = "Waviness vs Length (means per filopodium)",
		ylab = "Mean Waviness", xlab = expression("Mean Length [" * mu * "m]"),
		pch = 4, cex = 0.4, col = rgb(0,0,0,0.8))
	    abline(h = 0.38, col = rgb(1,0,0,0.5), lty = 3)	
	    abline(v = 1.8, col = rgb(1,0,0,0.5), lty = 3)
}


#--------------------------------------------------------------------------------
# Time Extending, Time Retracting, Time Stalling

# based on code in: /Users/Lab/Documents/Postdoc/ANALYSIS_local-files/BounderR/TimeSpentExtending/BounderR-dev_TimeExtending.R


pxw <- pxw  # USER-DEFINED AT TOP OF SCRIPT

threshold.ext.per.t = pxw
threshold.retr.per.t = -pxw

# TipState function. i) Absolute (number of timepoints in each state)

TipState.Abs <- function(x) {
	tip.state <- cut(x,
		breaks = c(-Inf, threshold.retr.per.t, threshold.ext.per.t, Inf),
		labels = c("Retr", "Stall", "Ext"))
	retrun(summary(tip.state))	
}

# TipState function. ii) Relative (proportion of time in each state)

TipState.Rel <- function(x) {
	tip.state <- cut(x,
		breaks = c(-Inf, threshold.retr.per.t, threshold.ext.per.t, Inf), 
		labels = c("Retr", "Stall", "Ext"))	
	return(summary(tip.state)/Count(tip.state))
	}

all.tip.states <- apply(all.fdctm, 2, TipState.Rel)


all.fdctm.post10 <- all.fdctm[post10, ]

all.tip.states.post10 <- apply(all.fdctm.post10, 2, TipState.Rel)

all.time.ext   <- all.tip.states["Ext", ]
all.time.stall <- all.tip.states["Stall", ]
all.time.retr  <- all.tip.states["Retr", ]

all.time.ext.post10 <- all.tip.states.post10["Ext", ]
all.time.stall.post10 <- all.tip.states.post10["Stall", ]
all.time.retr.post10 <- all.tip.states.post10["Retr", ]



#--------------------------------------------------------------------------------
# EXTRACTING BASE FLUORESCENCE SUMMARY METRICS per filopodium
#--------------------------------------------------------------------------------

# Define time intervals before formation:

bb   # number of base back-projection timepoints

# Indices for timepoints

last.1t <- c(bb)
last.3t <- c((bb-2):(bb))		# test on example: all.dT[,17][last.1t]
last.5t <- c((bb-4):bb)
last.10t <- c((bb-9):bb)

# all.base.nor[10:22, 1:10]
# all.base.nor[last.1t, 1:10]

mean.base.nor.1t <- apply(all.base.nor[last.1t, ], 2, mean); mean.base.nor.1t[non.new.from.short] <- NA
mean.base.nor.3t <- apply(all.base.nor[last.3t, ], 2, mean); mean.base.nor.3t[non.new.from.short] <- NA
mean.base.nor.5t <- apply(all.base.nor[last.5t, ], 2, mean); mean.base.nor.5t[non.new.from.short] <- NA
mean.base.nor.10t <- apply(all.base.nor[last.10t, ], 2, mean); mean.base.nor.10t[non.new.from.short] <- NA


#--------------------------------------------------------------------------------
# BounderR Module 2D - EXTRACTING TIP FLUORESCENCE DURING INTIIATION per filopodium
#--------------------------------------------------------------------------------

which(all.dT[1] == 1)

first.1t <- which(all.dT[1] == 1)
first.3t <- c(first.1t:(first.1t + 2))
first.5t <- c(first.1t:(first.1t + 4))
first.10t <- c(first.1t:(first.1t + 9))

# all.th.tip
# all.tip.nor2 
# 		[normalisation to body more suitable here than to projection:
#		very short projections, i.e. stronger total fluorescence contribution from the tip, bias]

mean.tip.nor.1t <- apply(all.tip.nor2[first.1t, ], 2, mean); mean.tip.nor.1t[non.new.from.short] <- NA
mean.tip.nor.3t <- apply(all.tip.nor2[first.3t, ], 2, mean); mean.tip.nor.3t[non.new.from.short] <- NA
mean.tip.nor.5t <- apply(all.tip.nor2[first.5t, ], 2, mean); mean.tip.nor.5t[non.new.from.short] <- NA
mean.tip.nor.10t <- apply(all.tip.nor2[first.10t, ], 2, mean); mean.tip.nor.10t[non.new.from.short] <- NA

mean.tip.th.1t <- apply(all.th.tip[first.1t, ], 2, mean); mean.tip.th.1t[non.new.from.short] <- NA
mean.tip.th.3t <- apply(all.th.tip[first.3t, ], 2, mean); mean.tip.th.3t[non.new.from.short] <- NA
mean.tip.th.5t <- apply(all.th.tip[first.5t, ], 2, mean); mean.tip.th.5t[non.new.from.short] <- NA
mean.tip.th.10t <- apply(all.th.tip[first.10t, ], 2, mean); mean.tip.th.10t[non.new.from.short] <- NA


setwd(Loc.save)

getwd()
save.image("CorrelationsInput.RData")  # <--- set filename for saved dataset

