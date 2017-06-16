#-------------------------------------------------------------------------------

# This script forms part of a set of analysis scripts for quantifying dynamic 
# behaviour of filopodia with the Fiji plugin Filopodyan. 

# More information available from: vu203@cam.ac.uk. 


#-------------------------------------------------------------------------------
# BounderR Module 1-2  -  BACKGROUND CORRECTION
#--------------------------------------------------------------------------


# This script implements background correction for fluorescence measurements.
# Downstream of (sourced by) 'Filopodyan Module 1.R'

#------------------------
# REQUIRED SCRIPT INPUT:

# Background correction setting:
# bg.corr.setting <- "none"  
	# "none":      no background correction (necessary if no Bodies table present)
	# "local":     use local background
	# "boundary":  use boundary background
	# "frame":     use frame background

# Data tables:
# - tip.table
#    - all.base
#    - all.tip
# - bodies.table (not obligatory for all bg settings)

#-----------------
# Quality control:
# Check dependencies:
stopifnot(exists("bg.corr.setting"))
stopifnot(exists("tip.table"))
#stopifnot(exists("tip.table"))
#-----------------

#--------------------------------------------------------------------------
# BODY F: Bg correction for body fluorescence measurements:

if(exists("bodies.table")) {
	body.bg.frame <- bodies.table[, grep(pattern = c("Frame Background"), 
		x = names(bodies.table), value = FALSE, fixed = TRUE)]
	body.bg.boundary <- bodies.table[, grep(pattern = c("Boundary Background"), 
		x = names(bodies.table), value = FALSE, fixed = TRUE)] 
	
	body.corrected.frame <- body.mean - body.bg.frame
	body.corrected.boundary <- body.mean - body.bg.boundary
}


#--------------------------------------------------------------------------
# BASE F & TIP F: Bg correction for base and tip fluorescence measurements:

if (bg.corr.setting == "local") {
	
	all.base.bg.local  <- tip.table[, grep(pattern = c("Base Local Background"), 
		x = names(tip.table), value = FALSE, fixed = TRUE)]
	
	all.base.corrected <- all.base - all.base.bg.local
	
	all.tip.bg.local <- tip.table[, grep(pattern = c("Tip Local Background"), 
		x = names(tip.table), value = FALSE, fixed = TRUE)]
	
	all.tip.corrected <- all.tip - all.tip.bg.local
		
}


if ((bg.corr.setting == "boundary") | (bg.corr.setting == "frame")) {
	
	#-------------------------------------------------------------------------
	# IMPLEMENT BACKGROUND CORRECTION USING THE MEASUREMENTS IN "BODIES" TABLE
	# The aim is to generate a table of body backgrounds with timepoints aligned according to dT
	# (for each filopodium in the Filopodia table).
	
	# This section is a bit tricky, but this is how it works:
	
	## 1. The ID system during table import keeps track of which filo comes from which GC
	
	## 2. These IDs are then used as reference to find the correct column in Bodies for
	# normalisation
	
	# Another layer of code is required to clean up the fact that measurements in Filopodia 
	# table e.g. Base Fluorescence are aligned according to dT, whereas the measurements in 
	# Bodies table e.g. Frame Background only have the T running along the rows of the table. 
	# The way around this is to read the information in the T column of the Filopodia table to 
	# identify which timepoints are needed from the Bodies table
	
	## 3. Finally, generate background tables that have all the columns aligned in the same way 
	
	## 4. Safety mechanisms are placed into the loop to trip the loop if any unexpected 
	# circumstances arising

	
all.bg.frame <- data.frame(matrix(NA, nrow = nrow(all.dT), ncol = ncol(all.dT)))
all.bg.boundary <- data.frame(matrix(NA, nrow = nrow(all.dT), ncol = ncol(all.dT)))
all.IDs <- data.frame(matrix(NA, nrow = nrow(all.dT), ncol = ncol(all.dT)))

all.base.raw <- all.base  # (for the purpose of transparency of the operations below)

for (i in seq_along(names(all.dT))) {
	
	# Safety check 1:
	stopifnot(ncol(all.dT) == ncol(all.base.raw)) # In case the dT import includes superfluous first column
	
	# Read movie IDs from the columns in all.dT
	column.ID <- readID(colnames(all.dT)[i])	
	corresponding.column <- grep(pattern = column.ID, x = colnames(body.bg.frame))
	
	if (i == 1) {column.IDs <- c()} 
	
	column.IDs <- append(column.IDs, column.ID)
	
	# A troubleshooting statement when multiple hits returned, e.g. "1" and "14" both including "1":
	if (length(corresponding.column) > 1) {
		# Take the hit with the shortest number of characters (e.g. "1" instead of "14" or "15")
	
		shortest <- which(nchar(colnames(body.bg.frame[, corresponding.column])) 
			== min(nchar(colnames(body.bg.frame[, corresponding.column]))))
	 	corresponding.column <- corresponding.column[shortest]	
		# alternatively, take the first matching ID! 
		# corresponding.column <- corresponding.column[1] 
	rm(shortest)
	}
	
	# Find non-empty rows in the dT column, and the respective timepoints
	rows.in.dT <- which(!is.na(all.dT[, i]) == TRUE)
	corresponding.T <- all.T[c(rows.in.dT), i]
	
	# Which timepoint rows in Bodies table correspond to the selected rows in all.T?
	rows.in.bodies <- body.T[, corresponding.column] %in% corresponding.T

	# Safety check 2 (body means should be identical whether from Filopodia table 
	# or Bodies table, if the subsetting is correct)
	safety.check <- 	body.mean[rows.in.bodies, corresponding.column]
	identical(safety.check, all.body[rows.in.dT, i])
	stopifnot(identical(safety.check, all.body[rows.in.dT, i]))
	
	# Find coresponding background measurements - using ID-matching column and matching rows
	corresponding.bg.frame <- body.bg.frame[rows.in.bodies, corresponding.column]
	corresponding.bg.boundary <- body.bg.boundary[rows.in.bodies, corresponding.column]
	all.bg.frame[rows.in.dT, i] <- corresponding.bg.frame
	all.bg.boundary[rows.in.dT, i] <- corresponding.bg.boundary
	all.IDs[rows.in.dT, i]      <- column.ID
	
	# Safety check 3 - after final iteration of loop
	if (i == length(names(all.dT))) {
		stopifnot(sum(!is.na(all.bg.frame)) == sum(!is.na(all.dT)))
		stopifnot(sum(!is.na(all.bg.boundary)) == sum(!is.na(all.dT)))
	}
	
	rm(rows.in.bodies, corresponding.bg.frame, corresponding.bg.boundary, corresponding.T, rows.in.dT, column.ID)
}

}

# all.bg.frame
# all.bg.boundary

if (bg.corr.setting == "boundary") {
	all.body.corrected = all.body - all.bg.boundary
	all.base.corrected = all.base - all.bg.boundary
	all.tip.corrected  = all.tip  - all.bg.boundary
	all.th.tip.corrected = all.th.tip - all.bg.boundary
	all.proj.corrected = all.proj - all.bg.boundary
}

if (bg.corr.setting == "frame") {
	all.body.corrected = all.body - all.bg.frame
	all.base.corrected = all.base - all.bg.frame
	all.tip.corrected  = all.tip  - all.bg.frame
	all.th.tip.corrected = all.th.tip - all.bg.frame
	all.proj.corrected = all.proj - all.bg.frame
}
