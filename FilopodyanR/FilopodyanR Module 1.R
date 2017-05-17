
#-------------------------------------------------------------------------------

# This script forms part of a set of analysis scripts for quantifying dynamic 
# behaviour of filopodia with the Fiji plugin Filopodyan. It is executed auto-
# matically by a parent script ("masterscript"), but requires parameter adjust-
# ments in the 'USER INPUT' section (lines 47+). For use independently of the
# masterscript, see 'manual input' section (lines 86-90) and remove/comment 
# lines 91-114.

# More information available from: vu203@cam.ac.uk. 

#-------------------------------------------------------------------------------
# FilopodyanR Module 1A  -  IMPORTING DATA TABLES 
#-------------------------------------------------------------------------------

# This script performs the following tasks: 

# MODULE 1A:  DATA IMPORT

# - Imports all data files in working directory named "... Filopodia.txt"
# - Extracts values for multiple parameters (e.g. Length, dL, DCTM) for each 
#     protrusion and combines them in data tables combining all measured
#     protrusions .
# - Stores the following variables within the current R environment:

#   - Length:  Length of structure 
#   - dL:      Change in length from previous timepoint
#   - DCTM:    Direction-corrected tip movement between adjacent timepoints
#                (DCTM>0: extension, DCTM<0: retraction)
#   - DCBM:    Direction-corrected base movement between adjacent timepoints 
#                (dB>0: distal movement, dB<0: proximal movement)
#   - TipF:	   Tip fluorescence  (thresholded pixel intensity)
# 	- BaseF:   Base fluorescence (mean pixel intensity)
#   - ProjF:   Projection fluorescence (mean pixel intensity)
#   - BodyF:   Body fluorescence


# MODULE 1B:  DATA CLEAN-UP 

# - Filters tip movement (DCTM) and base movement (DCBM) data so as
# 	  to exclude 1% of extreme values, (cutoff between 0.5th and 99.5th 
# 	  percentiles) which may have resulted from incorrect segmentation.
# - Smoothes the noise in DCTM values by applying rolling mean (generating 
#     'filtered DCTM' values, 'FDCTM')


#-------------------------------------------------------------------------------
# USER INPUT:  Edit this section.


# What is the rate of timelapse acquisition?
#rm(list = ls())
spt <- 2  # seconds per timepoint
pxw <- 0.065 # pixel width in microns

# How many base backprojected frames were calculated in each dataset?

bb <- 0

# Setting for background subtraction and normalisation

bg.corr.setting <- "boundary"  
	# "none":      no background correction (necessary if no Bodies table present)
	# "local":     use local background
	# "boundary":  use boundary background
	# "frame":     use frame background

# normalisation.mode.base <- "nor.to.body"  
	# "nor.to.body": normalise to body fluorescence
	# "none": 		 use non-normalised values (this is independent of bg subtraction)
	# nor.to.body is used as default currently, change requires editing script below
	
nor.tip.setting <- "nor.tip.to.body"
	# "nor.tip.to.filo": normalise to filo fluorescence
	# "nor.tip.to.body": normalise to body fluorescence
	# "none": 		 use raw values (this is independent of bg subtraction)
	
	
# END OF REQUIRED USER INPUT. 
# (alter the section below if using on single folder and not calling from the 
# masterscript for multiple folders)

#-------------------------------------------------------------------------------
# For manual input (single folder to analyse):

# setwd("")     #  <---- Set the directory filename containing your data and uncomment. Skip step below (automated input from parent script)
# e.g.:
#setwd("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_TOCA/Huang4-01") 
# Loc.modules  # <---- Set the directory of scripts for background correction.

# For automated scripted input of multiple folders (edit in parent script):

module.name <- "FilopodyanR Module1.R"
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
# Functions for tracking the identity of imported tables by filename:

extractID <- function(x) {
		
		# This function takes a string with numbers (file name) and concatenates digit characters, 	
		# separated by "-", e.g. extractID("s13_NeonCyto_GC8.tiff") returns "13-8", which goes into 
		# column headings to keep track of the origin of the movie.
		
		all <- unlist(strsplit(unlist(x), "[^0-9]")) 
		clean <- all[ !is.na(as.numeric(all)) ] 
		return(paste0(clean, collapse = "-")) 
		
		}	

readID <- function(x) {	
	
		# This function is complementary to extract ID: reads the ID string from table 
		# column headings and identifies the ID portion of it 
		# e.g. readID("dT (7)_15") returns "15"; 
		# e.g. readID(colnames(all.dT)[4])
	
	unlist(strsplit(x, split = "_"))[2]

}

#-------------------------------------------------------------------------------
# Read all data tables of "[...] filopodia.txt"

Loc <- getwd()

all.names <- list.files(as.character(Loc), pattern = "*Filopodia.txt")
all.names2 <- list.files(as.character(Loc), pattern = "*Coordinates.txt")
if(bg.corr.setting != 0) {
	all.names3 <- list.files(as.character(Loc), pattern = "*Bodies.txt")
}
# all.names; all.names2; all.names3

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

# As above, except measure max number of rows for the "... Bodies.txt" tables:

if (bg.corr.setting != 0) {
	rows.bodies <- vector(mode = "numeric", length = 0)	
	for (i in seq_along(all.names3)) {
		#print(all.names3[i])
		rows.bodies <- append(rows.bodies, nrow(read.table(all.names3[i], sep = "\t", skip = 1)))
		#print(rows.bodies[i])
	}
	max.t.bodies <- max(unlist(rows.bodies))
	max.t.bodies  # Max number of rows per table.
	rm(rows.bodies) 
}

# --- --- --- OPTIONAL USER INPUT: --- --- ---
# Insert a specifier for import (e.g. "CTRL" or "s09") in order to select only a 
# subset of tables for processing.

extract <- grep(pattern = c(""), x = all.names)  
#                            ^ Insert specifier inside "" (e.g. "CTRL"). 
extract

# --- --- --- END OF OPTIONAL USER INPUT: --- --- ---


# -------------------------------------------------------------------------------
# Define an ID for each source file	(e.g. ID = "10" for "GC10.tiff")
	
	IDs <- 	c()
	for (i in extract) {	IDs[i] <- extractID(all.names[i])}

# -------------------------------------------------------------------------------
# Import all data tables while keeping track of movie ID for each data table

# First import all " filopodia.txt" tables into a data frame called "tip.table": 

tip.table <- data.frame(matrix(NA, nrow = max.t, ncol = 0))

for (i in extract) {
	all.names[i]
	
		# Important addition for IDing columns:  # Added 8. feb 2017
		ID				= extractID(all.names[i]); IDs[i] <- ID
		vec      	   = readLines(all.names[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))
		# Important addition for IDing columns:
		header		   = paste(header, rep(ID, length(header)), sep = "_") 
		tab      	   = read.table(all.names[i], sep = "\t", skip = 1)
		colnames(tab)  = header
		top.up.rows    = max.t - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab), 
						nrow = top.up.rows))
		colnames(top.up.table) = header
		tip.table.i    = rbind(tab, top.up.table)
		# Filopodia table may include empty first column called dT! 
		superfluous    = which(colnames(tip.table.i) == paste0("dT_", ID))
		if(length(superfluous) > 0) {tip.table.i = tip.table.i[, -superfluous]}
		#tip.table <- tip.table[names(tip.table) != paste0("dT_", ID)]
		tip.table	   = cbind(tip.table, tip.table.i)	
		# Important line edit: include ID:
		rm(vec, header, tab, top.up.rows, top.up.table, tip.table.i, ID)			
}

tip.table[1:25, 1:9]  # Print top of the table for example filopodium 1.

#-------------------------------------------------------------------------------
# Coordinates import  # This section created 'bb' (base backprojections count) in previous versions, this is not accurate anymore, nor required. Add 'bb' elsewhere.

rows.c <- vector(mode = "numeric", length = 0)  
for (i in seq_along(all.names2)) {
	rows.c <- append(rows.c, nrow(read.table(all.names2[i], sep = "\t", skip = 1)))
}
max.c <- max(rows.c)

coord.table <- data.frame(matrix(NA, nrow = max.c, ncol = 0))
for (i in extract) {
	all.names2[i]
		ID			   = extractID(all.names2[i]); IDs[i] <- ID
		vec      	   = readLines(all.names2[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))		
		header		   = paste(header, rep(ID, length(header)), sep = "_") 
		tab      	   = read.table(all.names2[i], sep = "\t", skip = 1)
		colnames(tab)  = header
		top.up.rows    = max.c - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab),  
						nrow = top.up.rows))
		colnames(top.up.table) = header
		coord.table.i  = rbind(tab, top.up.table)
		# Coordinates table may include empty first column! (depending on version)
		superfluous    = which(colnames(coord.table.i) == paste0(" _", ID))
		if(length(superfluous) > 0) {coord.table.i = coord.table.i[, -superfluous]}
		coord.table	   = cbind(coord.table, coord.table.i)	
		rm(vec, header, tab, top.up.rows, top.up.table, coord.table.i, ID)			
}
coord.table[1:25, 1:12]

# Equalise number of rows between tip.table and coord.table
# comment on 15.03.2017: 

# (kept this section for compatibility with old versions of Bounder (before the 
# Coordinates table was made consistent with the Filopodia table in CAD-Bounder),
# however can no longer use this method to accurately set bb. Changed the assignment # of bb so this is now in the parent script (MASTERSCRIPT).

mm <- max.t - max.c   # mm for "mismatch" between the two tables, effectively the n of NA rows present in one but not the other (called 'bb' in previous versions)
if(mm > 0) {
	mm.table <- data.frame(matrix(NA, nrow = mm, ncol = ncol(coord.table)))
	colnames(mm.table) <- colnames(coord.table)
	coord.table <- rbind(mm.table, coord.table)
	rm(mm.table)
}
rm(mm)

#-------------------------------------------------------------------------------
# Bodies import

bodies.table <- data.frame(matrix(NA, nrow = max.t.bodies, ncol = 0))

if(exists("all.names3")) {
	for(i in extract) {

	all.names3
		ID			   = extractID(all.names3[i]); IDs[i] <- ID
		vec      	   = readLines(all.names3[i])[1]
		header   	   = unlist(strsplit(vec, "\t"))
		header		   = paste(header, rep(ID, length(header)), sep = "_") ; 
		
		tab      	   = read.table(all.names3[i], sep = "\t", skip = 1)
		top.up.rows	   = max.t.bodies - nrow(tab)
		top.up.table   = data.frame(matrix(NA, ncol = ncol(tab), 
						nrow = top.up.rows))
		colnames(tab)  = header
		colnames(top.up.table) = header
		bodies.table.i = rbind(tab, top.up.table)
		bodies.table   = cbind(bodies.table, bodies.table.i) 
		rm(vec, header, tab, bodies.table.i, ID)
	}	
}	

bodies.table[1:30, 1:14]


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

#----------------
# Quality control:
min.dT <- min(all.dT, na.rm = TRUE)
stopifnot(bb == abs(min.dT))  # min.dT should equal the specified bb. Check 'bb' on top of this script (Module 1)
#----------------
  
all.dS     <- all.dT * spt  # Seconds per timepoint defined on top of script.

dS.vector  <- apply(all.dS, 1, mean, na.rm=TRUE)

all.base   <- tip.table[, grep(pattern = c("Base Mean"), x = names(tip.table),
  value = FALSE, fixed = TRUE)]

all.body   <- tip.table[, grep(pattern = c("Body Mean"), x = names(tip.table),
  value = FALSE, fixed = TRUE)]
  
all.tip    <- tip.table[, grep(pattern = c("Tip Mean"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)] 

all.th.tip <- tip.table[, grep(pattern = c("Tip Th Mean"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]    # Added 17.8.

all.proj   <- tip.table[, grep(pattern = c("Proj"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]

all.length <- tip.table[, grep(pattern = c("Length"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dL     <- tip.table[, grep(pattern = c("dL"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dctm   <- tip.table[, grep(pattern = c("DCTM"), x = names(tip.table), 
  value = FALSE, fixed = TRUE)]
  
all.dcbm  <- tip.table[, grep(pattern = c("DCBM"), x = names(tip.table),  # Added 17.8.
  value = FALSE, fixed = TRUE)]
  
all.dB     <- all.dctm - all.dL

# From bodies table:

if(exists("bodies.table")) {
	
body.T <- bodies.table[, grep(pattern = c("T"), x = names(bodies.table), 
	value = FALSE, fixed = TRUE)]
body.mean <- bodies.table[, grep(pattern = c("Mean"), x = names(bodies.table), 
	value = FALSE, fixed = TRUE)]
}

#----------------
# Quality control:
min.dT <- min(all.dT, na.rm = TRUE)
stopifnot(bb == abs(min.dT))  # Number of base back-projected frames must match the min dT value in the all.dT table, otherwise hidden problems may occur later!
#----------------

#-------------------------------------------------------------------------------
# Background corrections:

if(bg.corr.setting != "none") {
	setwd(Loc.Modules)
	source("FilopodyanR Module 1-2_BgCorrection.R")
}

#-------------------------------------------------------------------------------
# Normalisations:

# from direct measuremetns without bg correction:

all.base.nor <- all.base / all.body
all.tip.nor1 <- all.tip  / all.proj  # normalise to projection intensity
all.tip.nor2 <- all.tip  / all.body  # normalise to GC body intensity
all.th.tip.nor1 <- all.th.tip  / all.proj  # normalise to projection intensity
all.th.tip.nor2 <- all.th.tip  / all.body  # normalise to GC body intensity
all.proj.nor <- all.proj / all.body 

# from bg corrected measurements:
if(bg.corr.setting != "none") {
	all.base.corr.nor = all.base.corrected / all.body.corrected 
	all.tip.corr.nor  = all.tip.corrected  / all.body.corrected
	all.th.tip.corr.nor1 = all.th.tip.corrected / all.proj.corrected
	all.th.tip.corr.nor2 = all.th.tip.corrected / all.body.corrected
	all.proj.corr.nor = all.proj.corrected / all.body.corrected
}

# See bottom of script for the definition of tip.f and all.move


#-------------------------------------------------------------------------------
# FilopodyanR Module 1B  -  DATA CLEAN-UP 
#-------------------------------------------------------------------------------

# The 99% filter for DCTM and dB values:
# Remove data outside the 0.5-99.5th percentiles for DCTM and DCBM. These are 
# likely to be erroneous due to large displacement of the tip when experiencing 
# drifting in and out of focus, or the length being fully/partially reconstructed. 
# Base displacement of very large magnitude occur when very small widening of the 
# protrusion near the base moves the asigned base position outwards (due to 
# discrete cutoff in base assignment during the segmentation process, as specified
# with ROI erosion-dilation [ED] cycles). 

# The smoothing filter for DCTM and DCBM values: 
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
all.dcbm99   <- RemoveOutliers(all.dcbm, dcbm.mincut, dcbm.maxcut)

#-------------------------------------------------------------------------------
# Create DCTM and DCBM value tables smoothed with a travelling mean filter 

MovingAverage <- function(x, w = 5) {
		filter(x, rep(1/w, w), sides = 2)
}

all.fdctm <- apply(all.dctm99, 2, MovingAverage)  # Mean-smoothed DCTM values after 99% filter
all.fdB   <- apply(all.dB99, 2, MovingAverage)   # Mean-smoothed dB values after 99% filter 
all.fdcbm   <- apply(all.dcbm99, 2, MovingAverage)   # Mean-smoothed dB values after 99% filter 

# To save tip movement tables:
# getwd()
# write.csv(x = all.fdctm, file = paste(condition, "DCTM-filtered.csv", sep = "_"))
# write.csv(x = all.fdcbm, file = paste(condition, "DCBM-filtered.csv", sep = "_"))


#-------------------------------------------------------------------------------
# For use with CCF scripts: 
# tip.f and all.move are the input for CCF calculations,
# for convenience define here which version of each to use.
	
if(nor.tip.setting == "nor.tip.to.filo") {
	
	if(bg.corr.setting == "none") {
		tip.f <- all.th.tip.nor1
	} else {
		tip.f <- all.th.tip.corr.nor1
	}
	cat("Tip fluorescence in 'tip.f' normalised to filopodium ('proj') fluorescence.")

} else if (nor.tip.setting == "none") {
	
	if(bg.corr.setting == "none") {
		tip.f <- all.th.tip
	} else {
		tip.f <- all.th.tip.corrected
	}
	cat("Tip fluorescence in 'tip.f' not normalised.")
		
} else if (nor.tip.setting == "nor.tip.to.body") {
	
	if(bg.corr.setting == "none") {
		tip.f <- all.th.tip.nor2
	} else {
		tip.f <- all.th.tip.corr.nor2
	}
	cat("Tip fluorescence in 'tip.f' normalised to body fluorescence.")
		
}

all.move <- all.fdctm

