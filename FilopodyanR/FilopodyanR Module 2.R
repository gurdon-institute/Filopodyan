#-------------------------------------------------------------------------------
# FilopodyanR Module 2  -  EXTRACTING DESCRIPTIVE METRICS per filopodium
#-------------------------------------------------------------------------------

# - Max Length							:   max.lengths
# - Median tip extension rate (DCTM +)  :	med.rate.extens
# - Median tip retraction rate (DCTM -) :	med.rate.retract
# - Median base invasion (DCBM +)		:	med.base.invas
# - Median base retraction (DCBM -)		: 	med.base.retract
# - Consistency of growth (the root of ACF) 
#										:	acf.fdctm.roots
# - Initial DCTM (new filo only)		:	dctm99.new.early.mean / .med
# - Initial DCBM (new filo)				:	dcbm99.new.early.mean / .med
# - Mean straightness					: 	straightness; waviness (1 - straightness)
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




# FILE MANAGEMENT:	
# Set the folders containing the data. See parent script 
# ("BounderR-structuresmanagement.R") for script input of multiple folders.

# For manual input (single folder to analyse):

# setwd("")  #  <---- Set the directory filename containing your data and uncomment.
# e.g. setwd("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/TOTAL-ANALYSIS_RENYI2-6_ED4") 

# For automated scripted input of multiple folders (edit in parent script):

module.name <- "FilopodyanR Module2.R"
initiate <- function() {

	if (exists("iter") == FALSE) {
		stop("Please define source folders in parent script.")
	} else { 

		if(iter == 1) {
			setwd(folder.names[1])
			print(module.name)
			print("EXECUTING ON:")
			print(folder.names[1])
		} else if(iter == 2) {
			setwd(folder.names[2])
			print(module.name)
			print("EXECUTING ON:")
			print(folder.names[2])
		} else print("Error: iter OUT OF RANGE, folders not defined. Get some fresh air. Make more folder names in parent script.")
	}
}

initiate()

getwd()
list.files()

#-------------------------------------------------------------------------------
# Max length


head(all.length)

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
# Altered from original (added MedianOfExtending etc.) on 15.3.2017

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

med.fdcbm.invas <- apply(all.fdcbm, 2, MedianOfExtending)  # 
med.fdcbm.retract <- apply(all.fdcbm, 2, MedianOfRetracting) #

# med.dB.invas <- apply(all.dB99, 2, MedianOfPositive)
# med.dB.retract <- apply(all.dB99, 2, MedianOfNegative) 


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

# Repeat for fdctm:  (added in this script on 15.3.2017, from correlations script)
acf.fdctm <- AcfTable(all.fdctm, 120)
acf.fdctm.roots <- apply (acf.fdctm, 2, FirstNegative)


#-------------------------------------------------------------------------------
# Extracting new vs pre-existing filopodia: 

# Old definition (superseded by newer min.T code below):
	# min.dT <- apply(all.dT, 2, min, na.rm=TRUE)
	# new <- which(min.dT < 0)
	# exist <- which(min.dT >= 0)

# New (changed 15.3.2017):

min.T <- apply(all.T, 2, min, na.rm=TRUE)
new <- which(min.T > 1)
exist <- which(min.T == 1)

# Add from.short filter:

cutoff = 2  #  <- Set cutoff in um for how short a filopodium needs to be in its first 
# timepoint in order to qualify as a new filopodium

from.short <- which(all.length[bb+1, ] < cutoff)
new.from.short <- which((min.T > 1) & (all.length[bb+1, ] < cutoff))   # Filopodia that come into existence, and <2um at t1
non.new.from.short <- which((!min.T > 1) | (!all.length[bb+1, ] < cutoff))  # Filo pre-existing or appearing into focus

from.short.cutoff <- cutoff; rm(cutoff)  # (for transparency of workspace)

#-------------------------------------------------------------------------------
# Initial DCTM and DCBM [over nt (10) timepoints] for new filopodia

nt <- 10  # Number of timepoints of interest post-formation
bb  # Base backprojections (variable defined in Module 1)
early <- (bb+1):(bb+1+nt); early  # vector of the timepoints required in this section 


dctm99.new.early.med <- apply(all.dctm99[early, new.from.short], 2, median, na.rm = TRUE)
dctm99.new.early.mean <- apply(all.dctm99[early, new.from.short], 2, mean, na.rm = TRUE) 

dcbm99.new.early.med <- apply(all.dcbm99[early, new.from.short], 2, median, na.rm = TRUE)
dcbm99.new.early.mean <- apply(all.dcbm99[early, new.from.short], 2, mean, na.rm = TRUE) 

fdctm.new.early.med <- apply(all.dctm99[early, new.from.short], 2, median, na.rm = TRUE)
fdctm.new.early.mean <- apply(all.dctm99[early, new.from.short], 2, mean, na.rm = TRUE)

fdcbm.new.early.med <- apply(all.fdcbm[early, new.from.short], 2, median, na.rm = TRUE)
fdcbm.new.early.mean <- apply(all.fdcbm[early, new.from.short], 2, mean, na.rm = TRUE)

#

if( draw.plots == TRUE) {
dev.new()
	hist(dctm99.new.early.mean,
		col = 'cadetblue', border = 'white',
		main = "Histogram of initial DCTM per filopodium",
		xlab = expression("DCTM [" * mu * "m / 2 s]") )
}

dB99.new.early.med <- apply(all.dB99[early, new.from.short], 2, median, na.rm = TRUE)
dB99.new.early.mean <- apply(all.dB99[early, new.from.short], 2, mean, na.rm = TRUE)

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
# (comment 15.3.2017: code for 'med.xxx.initial' copied from '2017-02-16_TOTAL-
# CTRL_Correlations', verify anew in this context!)

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

# CLEANUP required: straightness is not linear, strongly correlates with length up until 5 um  (artifact of the computation method). Solution: use only straightness at max length, for filopodia who reach max length over 5 um.
# (Added on 15.3.2017, code reused from 'Dataset1_CTRL2-6_Modules1-2_Manual.R' in '2017-02-16_TOTAL-CTRL_Correlations')

max.lengths <- apply(all.length, 2, max, na.rm = TRUE)

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

straightness.at.max.over5 <- straightness.at.max; straightness.at.max.over5[which(max.lengths < 5)] <- NA 

# Plotting:
# matplot(max.lengths, straightness.at.max, type = "p", pch = 16)
# matplot(max.lengths, straightness.mean, type = "p", pch = 16)
# abline(v = 4, col = "red")
# matplot(all.length, all.straightness, type = "p", pch = 4, col = "#00000008",
        # ylim = c(0,2), xlim = c(5,18))
    # abline(h = 1, lty = 3, col = 'grey')
# matplot(max.lengths, straightness.at.max.over5, type = "p", pch = 16)
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

all.time.ext   <- all.tip.states["Ext", ]
all.time.stall <- all.tip.states["Stall", ]
all.time.retr  <- all.tip.states["Retr", ]


#---------------------
# Base: Time Invading, Time Retracting, Time Stable  # ADDED ON 15.3.2017

pxw <- pxw  # USER-DEFINED AT TOP OF SCRIPT

threshold.inv.per.t = pxw
threshold.retr.per.t = -pxw

# BaseState function. i) Absolute (number of timepoints in each state)

BaseState.Abs <- function(x) {
	base.state <- cut(x,
		breaks = c(-Inf, threshold.retr.per.t, threshold.inv.per.t, Inf),
		labels = c("Retr", "Stall", "Ext"))
	retrun(summary(base.state))	
}

# BaseState function. ii) Relative (proportion of time in each state)

BaseState.Rel <- function(x) {
	base.state <- cut(x,
		breaks = c(-Inf, threshold.retr.per.t, threshold.inv.per.t, Inf), 
		labels = c("Retr", "Stable", "Inv"))	
	return(summary(base.state)/Count(base.state))
	}

all.base.states <- apply(all.fdcbm, 2, BaseState.Rel)

all.time.base.inv   <- all.base.states["Inv", ]
all.time.base.stable <- all.base.states["Stable", ]
all.time.base.retr  <- all.base.states["Retr", ]



