# This script uses base fluorescence data from FilopodyanR modules 1 and 1-2
# and plots Channel 1 vs Channel 2 Base Fluorescence (providing a number of 
# options for background correction). 

#-----------------------------------------------------------------
# IMPORT DATA (after analysis with masterscript)

rm(list = ls())
load('VASP-v-GAPRFP.Rdata')

# Check n-numbers match for the two datasets:
	# for n(filopodia) 
if((ncol(metalist[[1]]$all.length) == ncol(metalist[[2]]$all.length)) 
	# and n(GCs)
	& (metalist[[1]]$n.tables == metalist[[2]]$n.tables)) {

		cat("n numbers for filopodia and GCs match between two datasets: ")
			n.GC = metalist[[1]]$n.tables
			n.filo = ncol(metalist[[1]]$all.length)

		n.GC = metalist[[1]]$n.tables
		n.filo = ncol(metalist[[1]]$all.length)
		
		cat(paste0("n(filo) = ", n.filo, "  n(GC) = ", n.GC)
		)	
	} else {
		cat("PROBLEM: n numbers for filopodia and GCs don't match between the datasets")
	}


#-----------------------------------------------------------------
# Required functions and packages:

# Packages:
# install.packages('RColorBrewer', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(RColorBrewer)

# Functions:
Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
# CI <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))     

CI <- function (x, ci = 0.95) { 							
		
		# Using T distribution; 
		# Appropriate for small samples
		# ref: Rmisc package
		# Ryan M. Hope (2013). Rmisc: Rmisc: Ryan Miscellaneous. R package version 1.5. https://CRAN.R-project.org/package=Rmisc
		# citation("Rmisc")

		# My adjustments from Rmisc::CI:
		# 1. output as error value (not range around mean)
		# 2. n as Count(x) instead of length(x), so that NA values do not contribute to n
		
    a <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    n <- Count(x)
    error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
    return(error)
}

DrawErrorAsPolygon <- function(x, y1, y2, tt, col = 'grey') {
    polygon(c(x[tt], rev(x[tt])), c(y1[tt], rev(y2[tt])), 
    col = col,
    border = NA)			
    }

MovingAverage <- function(x, w = 5) {
		filter(x, rep(1/w, w), sides = 2)
}


#-----------------------------------------------------------------

# channel.1: 
dataset.names[1]

# channel.2:
dataset.names[2]

# Background correction
bg.settings = c(
	setting.1 = "all.base.nor.raw",
	setting.2 = "all.base.nor.corrected.frame",
	setting.3 = "all.base.nor.corrected.boundary",
	setting.4 = "all.base2.nor.corrected.frame",
	setting.5 = "all.base3.nor.corrected.boundary"
	)
	
bg.setting.names = c(
 setting.1 = "Base raw / GC raw",  # (original method)
 setting.2 = "Base local corr / GC frame corr",
 setting.3 = "Base local corr / GC boundary corr",
 setting.4 = "Base frame corr / GC frame corr",
 setting.5 = "Base boundary corr / GC boundary corr"
)


#-----------------------------------------------------------------
# GRAPHING FUNCTIONS:

DefYRange <- function(x,y) {
	
	min1 = min(min(x, na.rm = TRUE), min(y, na.rm = TRUE))
	max1 = max(max(x, na.rm = TRUE), max(y, na.rm = TRUE))	
	
	range1 = abs(max1 - min1)
	
	min2 = min1 - 0.3 * range1
	max2 = max1 + 0.5 * range1

	return(c(min2, max2))
}


GraphBaseF <- function(
				x1, x2, 
				error = "ci", 
				z.score = "FALSE", 
				cols = default.cols, 
				new = TRUE,
				...) {

	# x = all.base.nor, dataset1
	# y =  all.base.nor, dataset2
	
	if(z.score == "TRUE") {
		
		x1 <- scale(x1, center = TRUE, scale = TRUE)
		x2 <- scale(x2, center = TRUE, scale = TRUE)	
		y.label = "Norm. Base Fl. (z-score)"
	} else {
		x1 <- x1
		x2 <- x2
		y.label = "Normalised Base Fl."
	}
	
	x1.mean <- apply(x1, 1, mean, na.rm = TRUE)
	x2.mean <- apply(x2, 1, mean, na.rm = TRUE)

	
	if(error == "ci") {
		x1.ci <- apply(x1, 1, CI)
		x2.ci <- apply(x2, 1, CI)
	} else if(error == "sem") {
		x1.ci <- apply(x1, 1, SE)	
		x2.ci <- apply(x2, 1, SE)
	}	

#	cols <- c("#FFB266", "666666")
	# original colors: "#FFB266", "#00000040"
	# original transparencies: "#FFB26680", "#00000020"
	cols.tr <- paste0(cols, "40")

	
if(new == TRUE) {
#	dev.new(width = 5, height = 3)
		dev.new(width = 4, height = 3)
	} 
	par(mar = c(5,4,2,1)+0.1)

	x.range = 1:(spt*bb+6)
	y.range = DefYRange(x1.mean[x.range], x2.mean[x.range])

# Plot dataset 1
dS.vector <- metalist[[1]]$dS.vector
matplot(dS.vector[x.range], x1.mean[x.range],
	type = "l", 
	xlab = "Time [s]",
	ylab = y.label,
	ylim = y.range,
#	xlim = c(-spt*bb, spt*bb),   # EDIT ON 22 Feb 2017 (shorter X axis)
	xlim = c(-spt*bb, spt*3),
	cex.axis = 1.1,
	cex.lab = 1.1,
	lwd = 4, 
	col = cols[1],
	...
	)
ci.hi = x1.mean + x1.ci
ci.lo = x1.mean - x1.ci
DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, 0:44, col = cols.tr[1])
abline(v = 0, lty = 3)

# Plot dataset 2:
dS.vector <- metalist[[2]]$dS.vector
lines(dS.vector[x.range], x2.mean[x.range],
	lwd = 4,
	col = cols[2])	
 ci.hi = x2.mean + x2.ci
 ci.lo = x2.mean - x2.ci
 DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, 0:44, col = cols.tr[2])
 abline(v = 0, lty = 3)
	
}


brew.cols <- brewer.pal(5, "Accent")
#default.cols <- c("#FFB266", "#AAAAAA")
default.cols <- c("#DD0000", "#00DDDD")

GraphBaseF(
	metalist[[2]]$all.base3.nor.corrected.boundary, 
	metalist[[1]]$all.base3.nor.corrected.boundary,
	error = "ci"
)
# legend("topleft", legend = bg.setting.names[5], bty = "n")
legend("bottomright", legend = dataset.names[2], bty = "n", text.col = default.cols[1])
legend("topright", legend = dataset.names[1], bty = "n", text.col = default.cols[2])
legend("bottomleft", legend = paste0("n(filo) = ", n.filo, ", n(GC) = ", n.GC), bty = "n")

#---------------------------
# LOAD AND PLOT ENA DATASET:

load('ENA-v-GAPRFP.Rdata')
n.filo = 53
n.GC = 8

GraphBaseF(
	metalist[[2]]$all.base3.nor.corrected.boundary,
	metalist[[1]]$all.base3.nor.corrected.boundary,
	error = "ci"
)
# legend("topleft", legend = bg.setting.names[5], bty = "n")
legend("bottomright", legend = dataset.names[2], bty = "n", text.col = default.cols[1])
legend("topright", legend = dataset.names[1], bty = "n", text.col = default.cols[2])
legend("bottomleft", legend = paste0("n(filo) = ", n.filo, ", n(GC) = ", n.GC), bty = "n")



#-------------------------------------------------------------------------------
# Exploring different modes of background correction:


# (Orig raw) vs (Local Base / GC Frame)

# # GraphBaseF(
	# metalist[[1]]$all.base.nor.raw, metalist[[1]]$all.base.nor.corrected.frame,
	# error = "ci",
	# z.score = TRUE
	# #,cols = brew.cols[c(1,2)]
# )

# legend("bottomright", legend = bg.setting.names[1], bty = "n", text.col = default.cols[1])
# legend("topleft", legend = bg.setting.names[2], bty = "n", text.col = default.cols[2])


# # (Orig raw) vs (Local Base / GC Boundary)

# GraphBaseF(
	# metalist[[1]]$all.base.nor.raw, metalist[[1]]$all.base.nor.corrected.boundary,
	# error = "ci",
	# z.score = TRUE	
# )
# legend("bottomright", legend = bg.setting.names[1], bty = "n", text.col = default.cols[1])
# legend("topleft", legend = bg.setting.names[3], bty = "n", text.col = default.cols[2])

# # 3. (Orig raw) vs (Frame-corrected for both base and body)

# GraphBaseF(
	# metalist[[1]]$all.base.nor.raw, metalist[[1]]$all.base2.nor.corrected.frame,
	# error = "ci",
	# z.score = TRUE	
# )
# legend("bottomright", legend = bg.setting.names[1], bty = "n", text.col = default.cols[1])
# legend("topleft", legend = bg.setting.names[4], bty = "n", text.col = default.cols[2])

# # 4. (Orig Raw) vs (Boundary-corrected for both base and body)

# GraphBaseF(
	# metalist[[1]]$all.base.nor.raw, metalist[[1]]$all.base3.nor.corrected.boundary,
	# error = "ci",
	# z.score = TRUE	
# )
# legend("bottomright", legend = bg.setting.names[1], bty = "n", text.col = default.cols[1])
# legend("topleft", legend = bg.setting.names[5], bty = "n", text.col = default.cols[2])



# # Bg: no Bg correction:

# GraphBaseF(
	# metalist[[1]]$all.base.nor.raw,
	# metalist[[2]]$all.base.nor.raw,
	# error = "ci",
	# z.score = FALSE
# )
# legend("topleft", legend = bg.setting.names[1], bty = "n")
# legend("bottomright", legend = "NeonENA", bty = "n", text.col = default.cols[1])
# legend("topright", legend = "GAP-RFP", bty = "n", text.col = default.cols[2])

# # Bg: Settings 2

# GraphBaseF(
	# metalist[[1]]$all.base.nor.corrected.frame,
	# metalist[[2]]$all.base.nor.corrected.frame,
	# error = "ci",
	# z.score = FALSE
# )
# legend("topleft", legend = bg.setting.names[2], bty = "n")

# # Bg: Settings 3

# GraphBaseF(
	# metalist[[1]]$all.base.nor.corrected.boundary,
	# metalist[[2]]$all.base.nor.corrected.boundary
# )
# legend("topleft", legend = bg.setting.names[3], bty = "n")

# # Bg: Settings 4

# GraphBaseF(
	# metalist[[1]]$all.base2.nor.corrected.frame,
	# metalist[[2]]$all.base2.nor.corrected.frame
# )
# legend("topleft", legend = bg.setting.names[4], bty = "n")

# # Bg: Settings 5

# GraphBaseF(
	# metalist[[1]]$all.base3.nor.corrected.boundary,
	# metalist[[2]]$all.base3.nor.corrected.boundary
# )
# legend("topleft", legend = bg.setting.names[5], bty = "n")


# #dataset.names[1]
# #text(x = 44, y = 0.99, label = dataset.names[2], pos = 2, col = "#00000040")
# #text(x = 44, y = 1.03, label = dataset.names[1], pos = 2, col = "#FFB266")


# # For NeonENA, PLOT MEAN BASE, BASE BG (dT), FRAME BG (dT), BOUNDARY BG (dT)

# metalist[[1]]$new.from.short

# dev.new(width = 4.5, height = 6.3)
# par(mar = c(4,4,1,1)+0.1)
# matplot(all.dS[, new.from.short], all.base.raw[, new.from.short], 
	# type = "l",
	# lty = 1,
	# col = "#FFB26600",
	# ylim = c(0, 200),
	# xlim = c(-40, 40),
	# xlab = "dT [s]",
	# ylab = "Pixel intensity",
# # 	main = "Base Fluorescence during initiation",
	# main = "",
	# lab = c(6, 3, 7),
	# cex.main = 0.8,
	# cex.axis = 1
	# )
# #	title(xlab = "Time [s]", line = 2, cex.lab = 0.8)
# #	title(ylab = "Normalised Base Fluorescence", line = 2, cex.lab = 1)
# lines(dS.vector, bases.raw.mean,
	# lwd = 4,
	# col = "#FFB266")	
# ci.hi = bases.raw.mean + bases.raw.ci
# ci.lo = bases.raw.mean - bases.raw.ci
# DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, 0:44, col = "#FFB26680")
# abline(v = 0, lty = 3)

# # Line 2: local background measurements: 

# matplot(all.dS[, new.from.short], all.base.bg.local[, new.from.short],
	# type = "l",
	# lty = 1,
	# col = "#00000000",
	# add = TRUE)
# lines(dS.vector, local.base.bg.mean,
	# lwd = 4,
	# col = "#00000040")	
# ci.hi = local.base.bg.mean + local.base.bg.ci
# ci.lo = local.base.bg.mean - local.base.bg.ci
# DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, 0:44, col = "#00000020")
# abline(v = 0, lty = 3)

# # Line 3: background-corrected base measurements:
 
# matplot(all.dS[, new.from.short], all.base.corrected[, new.from.short],
	# type = "l",
	# lty = 1,
	# col = "#00000000",
	# add = TRUE)
# lines(dS.vector, bases.corrected.mean,
	# lwd = 4,
	# col = "#000000FF")	
# ci.hi = bases.corrected.mean + bases.corrected.ci
# ci.lo = bases.corrected.mean - bases.corrected.ci
# DrawErrorAsPolygon(dS.vector, ci.hi, ci.lo, 0:44, col = "#00000050")
# abline(v = 0, lty = 3)


