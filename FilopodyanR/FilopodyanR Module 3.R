### FilopodyanR Module 3: 

# Plotting of descriptive metrics (across different conditions).

# 3A: Plots box plots and CDFs for filopodium stats: 
	
	# Median tip extension rate	
	# Median tip retraction rate
	# Median base invasion rate
	# Median base retraction rate
	# Initial tip movement (new filo)
	# Initial base movement (new filo)
	# Max Length
	# Straightness at max length
	# Tip persistence	 	
	# Time spent extending
	# Time spent retracting
	# Time spent stalling	
		
# 3B: Timecourse graphs:

	# Length over time (new filo)
	# DCTM over time (new filo)
	# dB over time (new filo)

#------------------------------------------------------------------------------------------	
# IF NOT CALLING THIS SCRIPT FROM MASTERSCRIPT: LOAD DATA:
	
# load("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-02-15_s18_VASP_Phenotype/LastWorkspace_VASP-phenotype.Rdata")

# rm(list = ls())
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-04_s18_VASP_Phenotype/ANALYSIS_VASP-v-CTRL_Renyi2-6_ed4_MANUAL/LastWorkspace_Phenotype.Rdata')

# if 'reference.dataset' and 'dataset.other' are not yet specified (depending on upstream 
# code), define them here (they each need to match one of the 'dataset.names')

dataset.names
# reference.dataset <- "NeonCTRL" 
dataset.other <- dataset.names[which(dataset.names != reference.dataset)]

if(!exists("plot.boxCDFs")) {
	plot.boxCDFs  = TRUE
	plot.timecourse = FALSE
	plot.summary = TRUE
	save.summary = TRUE
}

#------------------------------------------------------------------------------------------
# Dependencies: 

# Packages:

library(purrr)
library(yarrr)
library(tidyr)
library(gplots)
library(RColorBrewer)
library(effsize)

# Modules:

setwd(Loc.Modules)  # <---- Set current working directory to the folder where 
					#		analysis scripts are located (correct this if needed)
					
source("GraphingTemplates.R")

# Functions:

Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
#CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }

#------------------------------------------------------------------------------

spt <- metalist[[1]]$spt

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Module 3A: Plot Box plots and CDFs (saves to folder.names[1])

if(plot.boxCDFs == TRUE) {

# Med. extension rate

dev.new()
par(mfrow = c(2,2))

x1 <- "med.rate.extens"

curr.data <- StandardGraphInput(x1, adjust.spt = "divide"); curr.data
Boxplot2(x1, 
    curr.title = "Median Extension Rate Per Filopodium",              # <---- Remember to edit here!
    curr.Ylab = expression("Median fDCTM (extending) [" * mu * "m/s]")  # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, adjust.spt = "divide",
	curr.title = "Median Extension Rate (CDF)",
	curr.Xlab = expression("Median fDCTM (extending) [" * mu * "m/s]") )

rm(x1, curr.data)


#------------------------------------------------------------------------------
# Med. retraction rate

x1 <- "med.rate.retract"

curr.data <- StandardGraphInput(x1, adjust.spt = "divide", flip = TRUE); curr.data
Boxplot2(x1, 
    curr.title = "Median Retraction Rate Per Filopodium",              # <---- Remember to edit here!
    curr.Ylab = expression("Median fDCTM (retracting) [" * mu * "m/s]")   # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, adjust.spt = "divide", flip = TRUE, 
	curr.title = "Median Retraction Rate (CDF)",
	curr.Xlab = expression("Median fDCTM (retracting) [" * mu * "m/s]"), 
	legend.where = "bottomright")
	
rm(x1, curr.data)

setwd(Loc.save)
dev.copy(pdf, "Rplot_BoxCDF_Parameters_01.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

#------------------------------------------------------------------------------
# Med base invasion

dev.new()
par(mfrow = c(2,2))

x1 <- "med.fdcbm.invas"

curr.data <- StandardGraphInput(x1, adjust.spt = "divide"); curr.data
Boxplot2(x1, 
    curr.title = "Median Invasion Rate Per Base",              # <---- Remember to edit here!
    curr.Ylab = expression("Median fDCBM (invading) [" * mu * "m/s]")  # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, adjust.spt = "divide",
	curr.title = "Median Base Invasion (CDF)",
	curr.Xlab = expression("Median fDCBM (invading) [" * mu * "m/s]") )

rm(x1, curr.data)

#------------------------------------------------------------------------------
# Med base retraction

x1 <- "med.fdcbm.retract"

curr.data <- StandardGraphInput(x1, adjust.spt = "divide", flip = TRUE); curr.data
Boxplot2(x1, 
    curr.title = "Median Retraction Rate Per Base",              # <---- Remember to edit here!
    curr.Ylab = expression("Median fDCBM (retracting) [" * mu * "m / 2 s]")   # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, adjust.spt = "divide", flip = TRUE,
	curr.title = "Median Base Retraction (CDF)",
	curr.Xlab = expression("Median fDCBM (retracting) [" * mu * "m / 2 s]"),
	legend.where = "bottomright" )
rm(x1, curr.data)

setwd(Loc.save)
dev.copy(pdf, "Rplot_BoxCDF_Parameters_02.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

# # #------------------------------------------------------------------------------
# # Initial DCTM

dev.new()

par(mfrow = c(2,2))

x1 <- "dctm99.new.early.med"

curr.data <- StandardGraphInput(x1, adjust.spt = "divide"); curr.data
Boxplot2(x1, 
    curr.title = "Initial DCTM in nascent filopodia",              # <---- Remember to edit here!
    curr.Ylab =  expression("DCTM (median for t = 1-10)  [" * mu * "m/s]")   # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, adjust.spt = "divide",
	legend.where = "topleft",
	curr.title = "Initial DCTM in nascent filopodia (CDF)",
	curr.Xlab = expression("DCTM (median for t = 1-10)  [" * mu * "m/s]")  )
rm(x1, curr.data)

# #------------------------------------------------------------------------------
# # Initial DCBM

x1 <- "dcbm99.new.early.med"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Initial Base Movement in nascent filopodia",              # <---- Remember to edit here!
    curr.Ylab =  expression("DCBM (median for t = 1-10)  [" * mu * "m / 2 s]")   # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, 
	legend.where = "topleft",
	curr.title = "Initial DCBM in nascent filopodia (CDF)",
	curr.Xlab = expression("DCBM (median for t = 1-10)  [" * mu * "m / 2 s]")  )

rm(x1, curr.data)

setwd(Loc.save)
dev.copy(pdf, "Rplot_BoxCDF_Parameters_03.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

# # #------------------------------------------------------------------------------
# # Initial DCTM vs dB (scatter)

# x1 <- "dctm99.new.early.med"
# y1 <- "dcbm99.new.early.med"

# x1 <- metalist[[1]]$"dctm99.new.early.med"
# y1 <- metalist[[1]]$"dcbm99.new.early.med"

# x2 <- metalist[[2]]$"dctm99.new.early.med"
# y2 <- metalist[[2]]$"dcbm99.new.early.med"


# # # curr.data.X <- StandardGraphInput(x1); curr.data.X
# # # curr.data.Y <- StandardGraphInput(y1); curr.data.Y

# # curr.cols$v 
# c1 <- c(curr.cols[, 1], 0.5)
# c2 <- c(curr.cols[, 2], 0.5)


# dev.new()
# plot(x1, y1, 
    # ylim = c(-0.5,0.5), xlim = c(-0.5,0.5),
    # type = "p", pch = 16, 
    # col = rgb(c1[1], c1[2], c1[3], 0.5),
    # main = "DCTM vs DCBM per filopodium over initial 10 timepoints",
	# xlab = expression("Mean DCTM [" * mu * "m / 2 s]"),
	# ylab = expression("Mean DCBM [" * mu * "m / 2 s]")
    # )
    # abline(h = 0, col = 'grey80', lty = 2)
    # abline(v = 0, col = 'grey80', lty = 2)
# points(x2, y2, type = "p", pch = 16, col = rgb(c2[1], c2[2], c2[3], 0.5))

#------------------------------------------------------------------------------
# Max length

dev.new()
par(mfrow = c(2, 2))

x1 <- "max.lengths"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Max Filopodium Length",              # <---- Remember to edit here!
    curr.Ylab = expression("Maximum filopodium length [" * mu * "m]")  # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Max Filopodium Lengths (CDF)",
	curr.Xlab = expression("Max length [" * mu * "m]")  )
	
rm(x1, curr.data)

#------------------------------------------------------------------------------
# Waviness

x1 <- "straightness.at.max.over5"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Straightness",  # <---- Remember to edit here!
    curr.Ylab = "Straightness at max length (>5 um) [au]"                     # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,	
	legend.where = "topleft",
	curr.title = "Straightness (CDF)",
	curr.Xlab = "Straightness at max length (>5 um)[au]"  )
	
rm(x1, curr.data)

setwd(Loc.save)
dev.copy(pdf, "Rplot_BoxCDF_Parameters_04.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

#------------------------------------------------------------------------------
# ACF DCTM roots

dev.new()
par(mfrow = c(2, 2))

x1 <- "acf.fdctm.roots"

curr.data <- StandardGraphInput(x1,adjust.spt = "multiply"); curr.data
Boxplot2(x1, 
    curr.title = "Root of fDCTM ACF (per filopodium)",              # <---- Remember to edit here!
    curr.Ylab =  "Time [s]"    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, adjust.spt = "multiply",
	curr.title = "Root of fDCTM ACF (CDF)",
	curr.Xlab = "Time [s]") 

#------------------------------------------------------------------------------
# ACF DCTM decay

max.L <- nrow(metalist[[1]]$acf.dctm)  # local variable (only this section)
acf.offset <- 1:max.L	# local

acf.dctm1.means <- apply(metalist[[1]]$acf.dctm, 1, mean, na.rm = TRUE)  # local
acf.dctm2.means <- apply(metalist[[2]]$acf.dctm, 1, mean, na.rm = TRUE)  # local
acf.dctm1.ci <- apply(metalist[[1]]$acf.dctm, 1, CI)  # local
acf.dctm2.ci <- apply(metalist[[2]]$acf.dctm, 1, CI)  # local

n1 <- ncol(metalist[[1]]$acf.dctm)
n2 <- ncol(metalist[[2]]$acf.dctm)

metalist[[1]]$acf.dctm[1:10, 1:10]

# Plot autocorrelation function for DCTM:

	matplot(acf.offset, acf.dctm1.means, 
		ylab = "ACF",
		xlab = "ACF offset (timepoints)",
		main = "Mean autocorrelation of DCTM",
		xlim = c(0, 50),
		type = 'l',
		lwd = 3, 
		col = rgb(t(curr.cols[1]))
		)
	matplot(acf.offset, acf.dctm2.means, 
		add = TRUE,
		type = 'l',
		lwd = 3, 
		col = rgb(t(curr.cols[2]))
		)
		
	# Draw error range for dataset1
	ci.hi1 = acf.dctm1.means + acf.dctm1.ci
	ci.lo1 = acf.dctm1.means - acf.dctm1.ci
	
	ci.hi2 = acf.dctm2.means + acf.dctm2.ci
	ci.lo2 = acf.dctm2.means - acf.dctm2.ci
	
	DrawErrorAsPolygon(acf.offset, ci.hi1, ci.lo1, tt = 1:(max.L-1), 
		col = rgb( t(curr.cols[1]), alpha = 0.2) )
	DrawErrorAsPolygon(acf.offset, ci.hi2, ci.lo2, tt = 1:(max.L-1), 
		col = rgb( t(curr.cols[2]), alpha = 0.2) )

	abline(h = 0, col = "black")
	
	legendstrings = paste(dataset.names, " (n = ", c(n1, n2), ")", sep = "")
	
	legend("topright", 
		legend = legendstrings,
		cex = 1,
		bty = "n",
		lwd = 3,
		col = rgb(t(curr.cols) )	
		)

rm(x1, curr.data, n1, n2, acf.offset, max.L, acf.dctm1.means, acf.dctm2.means, acf.dctm1.ci, acf.dctm2.ci, ci.hi1, ci.lo1, ci.hi2, ci.lo2)

setwd(Loc.save)
dev.copy(pdf, "Rplot_BoxCDF_Parameters_05.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()

#------------------------------------------------------------------------------
# Time spent extending

dev.new()
par(mfrow = c(3, 2))

x1 <- "all.time.ext"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time tip extending",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where tip speed > 0.0325 " * mu * "m/s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, 
	curr.title = "Time tip extending (CDF)",
	curr.Xlab = expression("Time where tip speed > 0.0325 " * mu * "m/s")
	)

#------------------------------------------------------------------------------
# Time spent retracting

x1 <- "all.time.retr"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time tip retracting",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where tip speed < -0.0325 " * mu * "m/s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, 
	curr.title = "Time tip retracting (CDF)",
	curr.Xlab = expression("Time where tip speed >0.0325 " * mu * "m/s")
	)

 #------------------------------------------------------------------------------
# Time spent stalling

x1 <- "all.time.stall"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time tip stalling",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where abs(FDCTM) < 0.0325 " * mu * "m / 2s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, 
	curr.title = "Time tip stalling (CDF)",
	curr.Xlab = expression("Time where abs(FDCTM) < 0.0325 " * mu * "m / 2s")
	)

setwd(Loc.save)
dev.copy(pdf, "Rplot_BoxCDF_Parameters_06.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()


# PLOT BASE MOVEMEMENT breakdown by time (ADDED 15.03.2017)

#------------------------------------------------------------------------------
# Time base invading

dev.new()
par(mfrow = c(3, 2))

x1 <- "all.time.base.inv"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time base invading",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where base speed > 0.0325 " * mu * "m/s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, 	
	curr.title = "Time base extending (CDF)",
	curr.Xlab = expression("Time where base speed > 0.0325 " * mu * "m/s")
	)

#------------------------------------------------------------------------------
# Time base retracting

x1 <- "all.time.base.retr"

curr.data <- StandardGraphInput(x1); curr.data
Boxplot2(x1, 
    curr.title = "Time base retracting",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where base speed < -0.0325 " * mu * "m/s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1,
	curr.title = "Time base retracting (CDF)",
	curr.Xlab = expression("Time where base speed < -0.0325 " * mu * "m/s")
	)

 #------------------------------------------------------------------------------
# Time base stable

x1 <- "all.time.base.stable"

curr.data <- StandardGraphInput(x1, adjust.spt = "multiply"); curr.data
Boxplot2(x1, 
    curr.title = "Time base stable",              # <---- Remember to edit here!
    curr.Ylab =  expression("Time where abs(FDCBM) < 0.0325 " * mu * "m/s")    # <---- Remember to edit here!
    )
legend("topright", QuickStats(x1), bty = "n", cex = 0.8)
CdfPlot2(x1, 
	curr.title = "Time base stable (CDF)",
	curr.Xlab = expression("Time where abs(FDCBM) < 0.0325 " * mu * "m/s")
	)

setwd(Loc.save)
dev.copy(pdf, "Rplot_BoxCDF_Parameters_07.pdf", 
		width = dev.size()[1], height = dev.size()[2])
	dev.off()
graphics.off()

}  # Plot all box plots and CDFs per parameter

 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Module 3B: Timecourse graphs

if(plot.timecourse == TRUE) {

	
	# For condition 1:
	 
	new <- metalist[[1]]$new
	metalist[[1]]$all.length[, new]
	
	new.lengths <- metalist[[1]]$all.length[, new]
	new.dctm    <- metalist[[1]]$all.dctm99[, new]
	new.dB    <- metalist[[1]]$all.dB99[, new]
	
	
	new.dS      <- metalist[[1]]$all.dS[, new]
	dS.vector   <- metalist[[1]]$dS.vector
	
	mean.new.lengths <- apply(new.lengths, 1, mean, na.rm = TRUE)
	mean.new.dctm    <- apply(new.dctm, 1, mean, na.rm = TRUE)
	mean.new.dB 	 <- apply(new.dB, 1, mean, na.rm = TRUE)
	
	median.new.lengths <- apply(new.lengths, 1, median, na.rm = TRUE) 
	median.new.dctm    <- apply(new.dctm, 1, median, na.rm = TRUE)
	median.new.dB    <- apply(new.dB, 1, median, na.rm = TRUE)
	
	ci.new.lengths    <-  apply(new.lengths, 1, CI) 
	ci.new.dctm       <-  apply(new.dctm, 1, CI) 
	ci.new.dB         <-  apply(new.dctm, 1, CI) 
	
	
	# problem: some of those newly in existence start off very long already!
	from.short <- which(new.lengths[21, ] < 2)
	
	# reassign the above accordingly: 
	
	mean.new.lengths <- apply(new.lengths[, from.short], 1, mean, na.rm = TRUE)
	mean.new.dctm    <- apply(new.dctm[, from.short], 1, mean, na.rm = TRUE)
	mean.new.dB    <- apply(new.dB[, from.short], 1, mean, na.rm = TRUE)
	
	ci.new.lengths    <-  apply(new.lengths[, from.short], 1, CI) 
	ci.new.dctm       <-  apply(new.dctm[, from.short], 1, CI) 
	ci.new.dB       <-  apply(new.dB[, from.short], 1, CI) 
	
	#------------------------------------------------------------------------------
	# 3B.i --- Plotting LENGTHS timecourse:
	
	dev.new()
	par(mfrow = c(2,2))
	matplot(new.dS[, from.short], new.lengths[, from.short],
		type = "l",
		lty = 1,
		col = "#00CCCC10",
		main = "Filopodium length over time",
		xlab = "Time [s]",
		ylab = expression("Length [" * mu * "m]"),
		xlim = c(0, 150),
		ylim = c(0, 10)
	)
	lines(dS.vector, mean.new.lengths,
		lwd = 4,
		col = "#00CCCC")	
	
	ci.hi = mean.new.lengths + ci.new.lengths
	ci.lo =	mean.new.lengths - ci.new.lengths
	
	DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, tt = 1:120, col = "#00CCCC40")
	
	abline(h = 0, col = "black", lty = 3)
	
	# head(new.lengths[20:26, ])
	
	
	# 3B.ii --- Plotting DCTM timecourse:
	
	
	matplot(new.dS[, from.short], new.dctm[, from.short],
		type = "l",
		lty = 1,
		col = "#66FF6610",
		main = "Tip movement over time",
		xlab = "Time [s]",
		ylab = expression("DCTM [" * mu * "m / 2 s]"),
		xlim = c(0, 60),
		ylim = c(-0.3, 0.3)
	)
	lines(dS.vector, mean.new.dctm,
		lwd = 4,
		col = "#66FF66" )
	ci.hi = mean.new.dctm + ci.new.dctm
	ci.lo =	mean.new.dctm - ci.new.dctm
		
	
	DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, tt = 1:120, col = "#66FF6640")
	
	abline(h = 0, col = "black", lty = 3)
	
	
	# 3B.iii --- Plotting DB timecourse:
	
	matplot(new.dS[, from.short], new.dB[, from.short],
	#	add = TRUE,
		type = "l",
		lty = 1,
		col = "#FFB26610",
		main = "Base movement over time",
		xlab = "Time [s]",
		ylab = expression("DB [" * mu * "m / 2 s]"),
		xlim = c(0, 60),
		ylim = c(-0.8, 0.8)
	)
	lines(dS.vector, mean.new.dB,
		lwd = 4,
		col = "#FFB266" )
	ci.hi = mean.new.dB + ci.new.dB
	ci.lo =	mean.new.dB - ci.new.dB
		
		
	DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, tt = 1:120, col = "#FFB26640")
	
	abline(h = 0, col = "black", lty = 3)
}  # Plot timecourse (for now only dataset 1)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Module 3C: Summary of phenotype 

# P values and adjustment:
spt= metalist[[1]]$spt
tidy_max.lengths 	<- StandardGraphInput("max.lengths") 
tidy_straightness.at.max.over5 <- StandardGraphInput("straightness.at.max.over5") 
tidy_med.rate.extens <- StandardGraphInput("med.rate.extens", adjust.spt = "divide") 
tidy_med.rate.retract 	 <- StandardGraphInput("med.rate.retract", adjust.spt = "divide", flip = TRUE) 
tidy_med.fdcbm.invas <- StandardGraphInput("med.fdcbm.invas", adjust.spt = "divide") 
tidy_med.fdcbm.retract  <- StandardGraphInput("med.fdcbm.retract", adjust.spt = "divide", flip= TRUE) 
tidy_all.time.ext  <- StandardGraphInput("all.time.ext") 
tidy_all.time.retr <- StandardGraphInput("all.time.retr") 
tidy_all.time.stall <- StandardGraphInput("all.time.stall") 
tidy_all.time.base.inv  <- StandardGraphInput("all.time.base.inv") 
tidy_all.time.base.retr <- StandardGraphInput("all.time.base.retr") 	
tidy_all.time.base.stable <- StandardGraphInput("all.time.base.stable") 
tidy_acf.fdctm.roots <- StandardGraphInput("acf.fdctm.roots", adjust.spt = "multiply") 

#tidy_var.all.move  <- StandardGraphInput("var.all.move") 
#tidy_n.timepoints,
#tidy_acf.tip.f.roots,
#tidy_var.tip.f,
#tidy_mean.f.proj.raw,
#tidy_mean.f.body.raw,
#tidy_mean.f.tip.raw,
#tidy_mean.f.proj,
#tidy_mean.f.body,
#tidy_mean.f.tip

myquantnames <- c(
	"max.lengths",
	"straightness.at.max.over5",
	"med.rate.extens",
	"med.rate.retract",
	"med.fdcbm.invas",
	"med.fdcbm.retract",
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

myquant <- list(
	tidy_max.lengths,
	tidy_straightness.at.max.over5,
	tidy_med.rate.extens,
	tidy_med.rate.retract,
	tidy_med.fdcbm.invas,
	tidy_med.fdcbm.retract,
	tidy_all.time.ext,
	tidy_all.time.retr,
	tidy_all.time.stall,
	tidy_all.time.base.inv,
	tidy_all.time.base.retr,	
	tidy_all.time.base.stable,
	tidy_acf.fdctm.roots
	# ,tidy_var.all.move,
	# tidy_n.timepoints,
	# tidy_acf.tip.f.roots,
	# tidy_var.tip.f,
	# tidy_mean.f.proj.raw,
	# tidy_mean.f.body.raw,
	# tidy_mean.f.tip.raw,
	# tidy_mean.f.proj,
	# tidy_mean.f.body,
	# tidy_mean.f.tip
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
myquant_abbrev <- list(
	"Len",		# tidy_max.lengths,
	"Str",		# tidy_straightness.at.max.over5,
	"TExt",		# tidy_med.rate.extens,
	"TRet",		# tidy_med.rate.retract,
	"BInv",		# tidy_med.fdcbm.invas,
	"BRet",	# tidy_med.fdcbm.retract,
	"TTE",		# tidy_all.time.ext,
	"TTR",		# tidy_all.time.retr,
	"TTS",		# tidy_all.time.stall,
	"TBI",		# tidy_all.time.base.inv,
	"TBR",		# tidy_all.time.base.retr,	
	"TBS",			# tidy_all.time.base.stable,
	"Per"			# tidy_acf.fdctm.roots
			# # ,tidy_var.all.move,
			# # tidy_n.timepoints,
			# # tidy_acf.tip.f.roots,
			# # tidy_var.tip.f,
			# # tidy_mean.f.proj.raw,
			# # tidy_mean.f.body.raw,
			# # tidy_mean.f.tip.raw,
			# # tidy_mean.f.proj,
			# # tidy_mean.f.body,
			# # tidy_mean.f.tip
)

# Check all are there:
lapply(myquant, function(x) dim(x))

# Check matching:
data.frame("Parameter code" = myquantnames, "Full name" = myquant_fullnames, "Abbreviation" = unlist(myquant_abbrev))

names(myquant) <- myquant_fullnames

#-------------------------------------------------------------------------------
# Adjusted P-values: 

for(i in 1:length(myquant)) {
	stopifnot(length(myquant) > 0)
	if(i == 1) {
			myquant.multi.mw.test <- list() 
			myquant.multi.p <- list() 
			}
	z <- myquant[[i]]
	myquant.multi.mw.test[[i]] <- wilcox.test(Value ~ Source, data = z)
	myquant.multi.p[[i]] <- wilcox.test(Value ~ Source, data = z)$p.value
}
rm(z)

adjusted.p.values <- data.frame(
		"var" = myquantnames, 
		"Mann-Whitney" = unlist(myquant.multi.p), 
		"adj Bonf" = p.adjust(unlist(myquant.multi.p), method = "bonferroni"),
		"adj Holm" = p.adjust(unlist(myquant.multi.p), method = "holm")
		)

print(adjusted.p.values)

setwd(Loc.save)
write.csv(adjusted.p.values, "Adjusted-p.csv")

#-------------------------------------------------------------------------------
# Compute z scores

FindZScore <- function(x1, x2) {
	mean.x1 <- mean(x1, na.rm = TRUE)
	mean.x2 <- mean(x2, na.rm = TRUE)	
	sd.x1 <- sd(x1, na.rm = TRUE)
	sd.x2 <- sd(x2, na.rm = TRUE)
	
	z1 <- (mean.x2 - mean.x1) / sd.x1
	z2 <- (mean.x1 - mean.x2) / sd.x2
	return(c(z1, z2))  # z1 expresses change of dataset2 compared to dataset1
						# z2 expresses change of dataset1 compared to dataset2
}

FindZScores <- function(x) {
	x1 <- subset(x, Source == reference.dataset)$Value
	x2 <- subset(x, Source == dataset.other)$Value
	mean.x1 <- mean(x1, na.rm = TRUE)
	mean.x2 <- mean(x2, na.rm = TRUE)	
	sd.x1 <- sd(x1, na.rm = TRUE)
	sd.x2 <- sd(x2, na.rm = TRUE)
	z1 <- (mean.x2 - mean.x1) / sd.x1  # z1 expresses change of 'other'dataset relative to 'reference'
	z2 <- (mean.x1 - mean.x2) / sd.x2  # z2 is vice versa
	z <- c(z1, z2)
	names(z) <- c(paste(dataset.other, "vs", reference.dataset),
				  paste(reference.dataset, "vs", dataset.other)
				  )
	return(z)
}

z.scores <- lapply(myquant, FindZScores)
names(z.scores) <- names(myquant)
z.scores

#-------------------------------------------------------------------------------
# Compute Cliff's delta:

# delta.scores <- lapply(myquant, function(x) cliff.delta(Value ~ Source, data = x)$estimate) 
# cohen.scores <- lapply(myquant, function(x) {cohen.d(formula = Value ~ Source, data = x)$estimate})
# these two above would mess up the sign (+ v -) dep. on the names of datasets (alphabetically)

stopifnot(exists("reference.dataset"))
stopifnot(exists("dataset.other"))

delta.scores <- lapply(myquant, function(x) cliff.delta(
	Value ~ factor(x$Source, levels = c(dataset.other, reference.dataset)), 
	data = x)$estimate)
delta.scores 

delta.matrix <- matrix(NA, nrow = 2, ncol = length(myquant))
	delta.matrix[1, ] <- unlist(transpose(delta.scores)[[1]])
	delta.matrix[2, ] <- unlist(transpose(delta.scores)[[1]])
	colnames(delta.matrix) <- names(myquant)


#-------------------------------------------------------------------------------
# SUMMARY PLOTS

if(plot.summary == TRUE) {
library(RColorBrewer)
curr.pal = brewer.pal(11, "PRGn")
curr.pal
scale.edges = c(-1, 1)
scale.breaks <- seq(scale.edges[1], scale.edges[2], length.out = length(curr.pal)+1)

#-------------------------------
# PLOT CLIFF'S DELTA AS HEATMAP:

# dev.new(width =7, height = 3.5)
# heatmap.2(delta.matrix, 
	# # Switch off reordering and dendrograms:
      # Colv = FALSE, Rowv = FALSE, dendrogram = "none", 
      # breaks = scale.breaks,
      # symkey = F,
    # # Switch off other bells and whistles:
      # trace = "none",
    # # Color:
      # col = curr.pal,
    # # Separation:
      # colsep = c(1:23),  #rowsep = c(1:23),
      # sepcol = "white", sepwidth = c(0.0, 0.02),
    # # Legend: 
      # keysize = 2, density.info = "none", key.title = "",
      # key.xlab = "Effect size",
    # # Labels:
      # labRow = "", # dataset.names[c(2,1)], 
      # cexRow = 1, cexCol = 0.8,
      # srtCol = 45
    # # Layout:
    # # Note within cells:
    # # cellnote = rep(as.character(1:23), 2)
# )
# setwd(Loc.save)
# dev.copy(pdf, "Rplot_PhenotypeSummary_Heatmap_CliffDelta.pdf", 
		# width = dev.size()[1], height = dev.size()[2])
	# dev.off()
	
	
#------------------------------------------------------------------------------
# PIRATEPLOTS (PHENOTYPE SUMMARY PLOTS):

# 1. Normalise all data:

ScaleToRef <- function(x, reference.dataset) {
	
	ref.subset <- subset(x, subset = (Source == reference.dataset))

	ref.mean <- mean(ref.subset$"Value", na.rm = TRUE)
	ref.sd <- sd(ref.subset$"Value", na.rm = TRUE)

	x$"Scaled" <- (x$"Value" - ref.mean) / ref.sd
	x
}

ScaleToRef_MedIQR <- function(x, reference.dataset) {
	
	ref.subset <- subset(x, subset = (Source == reference.dataset))

	ref.median <- median(ref.subset$"Value", na.rm = TRUE)
	ref.iqr <- IQR(ref.subset$"Value", na.rm = TRUE) 

	x$"Scaled" <- (x$"Value" - ref.median) / ref.iqr
	x
}

myquant.scaled <- lapply(myquant, function(x) ScaleToRef(x, reference.dataset))
myquant.scaled_2 <- lapply(myquant, function(x) ScaleToRef_MedIQR(x, reference.dataset))

myquant.ref <- lapply(myquant.scaled, function(x) subset(x, subset = (x$Source == reference.dataset)))
myquant.other <- lapply(myquant.scaled, function(x) subset(x, subset = (x$Source != reference.dataset)))

myquant.ref2 <- lapply(myquant.scaled_2, function(x) subset(x, subset = (x$Source == reference.dataset)))
myquant.other2 <- lapply(myquant.scaled_2, function(x) subset(x, subset = (x$Source != reference.dataset)))

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

# example:
# CombineAllParameters(myquant.ref, column = "Scaled")
# CombineAllParameters(myquant.other, column = "Scaled")

# Diagnostic tools
# lapply(myquant.scaled, function(x) c(summary(x$Scaled), "SD" = sd(x$Scaled, na.rm = TRUE)))
# by(data = x$Scaled, INDICES = x$Source, FUN= function(y) {c("mean" = mean(y, na.rm = TRUE),"sd" = sd(y, na.rm = TRUE))})

# PRINT THE SUMMARY OF DATA:
apply(CombineAllParameters(myquant.ref, column = "Scaled"), 2, function(x) c(summary(x), "SD" = sd(x, na.rm = TRUE)))
apply(CombineAllParameters(myquant.other, column = "Scaled"), 2, function(x) c(summary(x), "SD" = sd(x, na.rm = TRUE)))
apply(CombineAllParameters(myquant.ref2, column = "Scaled"), 2, function(x) c(summary(x), "SD" = sd(x, na.rm = TRUE)))
apply(CombineAllParameters(myquant.other2, column = "Scaled"), 2, function(x) c(summary(x), "SD" = sd(x, na.rm = TRUE)))

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
		point.o = 0.00,  # <--- change points opacity
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

# Plot using means-SD scaling:

#  ... with CTRL data (reference.dataset):

dev.new(width = 5, height = 8)
	PiratePlot(key~value, data = gather(CombineAllParameters(myquant.ref, column = "Scaled")),
		inf.method = "ci",
		avg.line.lwd = 2
	)
	rect(par("usr")[1]-1, ybottom = -1, par("usr")[2]+1, ytop = 1, border = "#00000025")
	mtext("Mean", side = 2, at = 0, cex = 0.8, srt = 90, line = 1)
	dev.copy(pdf, width = dev.size()[1], height = dev.size()[2], paste0("Rplot_Pirateplot_", reference.dataset, "_MeanCI_SD.pdf")); dev.off()

#  ... with other dataset (dataset.other - e.g. "NeonVASP"):

dev.new(width = 5, height = 8)
	PiratePlot(key~value, data = gather(CombineAllParameters(myquant.other, column = "Scaled")),
		inf.method = "ci",
		avg.line.lwd = 2)
	rect(par("usr")[1]-1, ybottom = -1, par("usr")[2]+1, ytop = 1, border = "#00000025")
	dataset.other <- dataset.names[which(dataset.names != reference.dataset)]
	mtext("Mean", side = 2, at = 0, cex = 0.8, srt = 90, line = 1)
	dev.copy(pdf, width = dev.size()[1], height = dev.size()[2], paste0("Rplot_Pirateplot_", dataset.other, "_MeanCI_SD.pdf")); dev.off()

#  ... with other dataset (dataset.other - e.g. "NeonVASP"):

dev.new(width = 5, height = 8)
	PiratePlot(key~value, data = gather(CombineAllParameters(myquant.other, column = "Scaled")),
		inf.method = "ci",
		avg.line.lwd = 2,
		inf.opacity = 0.5,
		inf.f.col = as.character(cut(unlist(delta.scores), breaks = 0.5*scale.breaks, labels = curr.pal))
		)
	rect(par("usr")[1]-1, ybottom = -1, par("usr")[2]+1, ytop = 1, border = "#00000025")
	dataset.other <- dataset.names[which(dataset.names != reference.dataset)]
	mtext("Mean", side = 2, at = 0, cex = 0.8, srt = 90, line = 1)
	dev.copy(pdf, width = dev.size()[1], height = dev.size()[2], paste0("Rplot_Pirateplot_", dataset.other, "_MeanCI_SD_Color.pdf")); dev.off()


# Plot using median-0.5 IQR scaling:

#  ... with CTRL:

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

#  ... with VASP/other:

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
		inf.f.col = "lightgrey" #as.character(cut(unlist(delta.scores), breaks = 0.5*scale.breaks, labels = curr.pal))
		)
	rect(par("usr")[1]-1, ybottom = -0.5, par("usr")[2]+1, ytop = 0.5, border = "#00000025")
	mtext("Median", side = 2, at = 0, cex = 0.8, srt = 90, line = 1)

	dev.copy(pdf, width = dev.size()[1], height = dev.size()[2], paste0("Rplot_Pirateplot_", dataset.other, "_Median_halfIQR.pdf")); dev.off()
	
# ... IN COLOUR:

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


#----------------------------
# Plot P values (-log(base=10))

dev.new(width = 7, height = 1.5)
	adj.p <- adjusted.p.values$adj.Holm; names(adj.p) <- unlist(myquant_abbrev)
	par(mar = c(3,4,1,1) + 0.1)
	barplot(-log(adj.p, base = 10), ylim = c(0, 3), ylab = "-log(P)", 
		cex.names = 0.8)
	
	abline(h = -log(0.05, base = 10), lty = 3)
	setwd(Loc.save)
	dev.copy(pdf, "Rplot_P-values_adj.pdf", width = dev.size()[1], height = dev.size()[2]); dev.off()
  # End of plot.summary section
}

#------------------------------------------------------------------------------
# Create a summary to export as a .csv file

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

rm(z)
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
z$"Condition" <- rep(dataset.names, nrow(z)/2)
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
#   - fold changes
#   - P value (Mann-Whitney before adjustment)
#   - P value (with Holm adjustment)
#   - z score (for each dataset, relative to the other dataset)
#   - Cliff's delta

MedianFoldChange <- function(x) {
	ref <- which(x$Source == reference.dataset)
	other <- which(x$Source != reference.dataset)
	m.ref <- median(x$Value[ref], na.rm = TRUE)
	m.other <- median(x$Value[other], na.rm = TRUE)
 	return (c(m.other/m.ref, m.ref/m.other))
}

MeanFoldChange <- function(x) {
	ref <- which(x$Source == reference.dataset)
	other <- which(x$Source != reference.dataset)
	m.ref <- mean(x$Value[ref], na.rm = TRUE)
	m.other <- mean(x$Value[other], na.rm = TRUE)
 	return (c(m.other/m.ref, m.ref/m.other))
}

fold.changes.medians <- lapply(myquant, MedianFoldChange)
fold.changes.means <- lapply(myquant, MeanFoldChange)
fold.changes.medians

# Quality check:
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
graphics.off()
