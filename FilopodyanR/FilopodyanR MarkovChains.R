# TIP FLUORESCENCE & MOVEMENT - Markov chains

# This script is part of a suite of scripts for analysis of filopodia dynamics 
# using the Fiji plugin Filopodyan. 
#
# Data input: requires an .Rdata file from upstream script 
# ('Filpodyan CCF_subcluster-analysis.R') 
# Data output: a large number of Markov chains based the original dataset (for each 
#    filopodium, 10.000 chains for tip movement and 10.000 chains for fluorescence), and
#    a summary of their properties relative to the original data: how many simulations 
#    for each filopodium have a better correlation between movement and fluorescence than 
#    the original dataset of that filopodium, and how many SDs away from the mean of the 
#    pool of simulations is the observed real dataset for each filopodium

rm(list = ls())

#--------------------------------------------------------------------------------
# Install req package:

#install.packages('DTMCPack', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('markovchain', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages('animation', dependencies=TRUE, repos='http://cran.rstudio.com/') 
# from: https://www.r-bloggers.com/r-animating-2d-and-3d-plots/ 

library(DTMCPack)
library(markovchain)
# library(animation)


#--------------------------------------------------------------------------------
# Load data:

 load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toGC/LastWorkspace_CCF_Subclusters.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC/LastWorkspace_CCF_Subclusters.Rdata')

#--------------------------------------------------------------------------------
# 1. Required functions:

FirstNonNA <- function(x) {
	nonNA.index <- which(!is.na(x))
	first.nonNA <- min(nonNA.index, na.rm = TRUE)
	last.nonNA <- max(nonNA.index, na.rm = TRUE)
	return(first.nonNA)
}
LastNonNA <- function(x) {
	nonNA.index <- which(!is.na(x))
	last.nonNA <- max(nonNA.index, na.rm = TRUE)
	return(last.nonNA)
}
NonNArange <- function(x) {   	# Doesn't clean up NAs within the time series! (intentionally)
	FirstNonNA(x):LastNonNA(x)
	}

GetSDfromMean <- function(vector, value) {
			mean <- mean(vector, na.rm = TRUE)
			sd <- sd(vector, na.rm = TRUE)			
			z <- (value-mean)/sd
			z
		}	

SimulateFilo <- function(y, x, n.sim = n.sim, output = "count", ...) {
	
	stopifnot(length(x) == length(y))	
	
	#----
	# 1. Prepare required elements and Markov Chain properties:

	y1 <- y[NonNArange(y)]
	x1 <- x[NonNArange(x)]
	
	states.y = c(1:9)
	states.x = c(1:9) # this will need upgrading to handle NAs too 
	y.d <- cut(y1, 9, labels = states.y) # 'd' for discrete
	x.d <- cut(x1, 9, labels = states.x)

	# Solving the NAs problem:
	
	if(sum(is.na(y.d)) > 0) {
		states.y[10] <- NA
	}

	if(sum(is.na(x.d)) > 0) {
		states.x[10] <- NA
	}

	tMatrixY <-  markovchainFit(y.d)$estimate@transitionMatrix
	tMatrixX  <- markovchainFit(x.d)$estimate@transitionMatrix

	mcY <- new("markovchain", 
		states = as.character(colnames(tMatrixY)), 
		byrow = TRUE, 
		transitionMatrix = tMatrixY,
		name = "Movement")

	mcX <- new("markovchain", 
		states = as.character(colnames(tMatrixX)), 
		byrow = TRUE, 
		transitionMatrix = tMatrixX,
		name = "Fluorescence")
	
	#----
	# 2. Generate <n.sim> simulations
		
	sim.rho <- rep(NA, length = n.sim)
	sim.p <- rep(NA, length = n.sim)
	sim.move <- data.frame(matrix(NA, ncol = n.sim, nrow = length(y)-bb))
	sim.tip  <- data.frame(matrix(NA, ncol = n.sim, nrow = length(x)-bb))
	
	orig.rho <- cor.test(as.numeric(y), as.numeric(x), method = "spearman")$estimate
	orig.p <- cor.test(as.numeric(y), as.numeric(x), method = "spearman")$p.value
		
	for (i in 1:n.sim) {
		
		move.sim.i <- as.integer(rmarkovchain(121, object = mcY, t0 = y.d[1]))
		tip.sim.i <- as.integer(rmarkovchain(121, object = mcX, t0 = x.d[1]))	
	
		sim.move[, i] <- move.sim.i
		sim.tip[, i] <- tip.sim.i
		
		p.i <- cor.test(as.numeric(move.sim.i), as.numeric(tip.sim.i), 
				method = "spearman")$p.value
		rho.i <- cor.test(as.numeric(move.sim.i), as.numeric(tip.sim.i), 
				method = "spearman")$estimate
		
		sim.p[i] <- p.i 
		sim.rho[i] <- rho.i 
	}
	
	#----
	# 3. Compare all simulations with orig. x & y

	# 3a. Output how many simulations were as correlated as the original
	
	
		count <- Count(which(sim.rho > orig.rho))
	
	# 3b. output how many SDs away from mean
		sd <- GetSDfromMean(sim.rho, orig.rho)

		print(c("count" = count, "sd" = sd))

	# 3c. output: complete
		if(output == "complete") {
			z <- list()
			z$count = count			
			z$sd = sd
			z$sim.move = sim.move
			z$sim.tip = sim.tip
			z$sim.p = sim.p
			z$sim.rho = sim.rho
			
			return(z)
		} else {	
			return(c("count" = count, "sd" = sd))
			}

}

# Not a fast loop! I appreciate suggestions for speeding it up...


#--------------------------------------------------------------------------------
# Run simulations for each filopodium, and compare with original on the go.
		
		
n.filo = ncol(all.move)
	
simulation.count <- rep(NA, n.filo)
simulation.SDs <- rep(NA, n.filo)

SimulateFilo(all.move[,3], tip.f[,3], n.sim = 100)

n.sim = 10  # Troubleshooting only, otherwise:
# n.sim = 10000



for(f in 1:n.filo) {
# (Troubleshooting errors)
# for(f in c(35, 37, 39, 41, 45)) {   
# f = 35

	z <- try(SimulateFilo(all.move[, f], tip.f[, f], n.sim = n.sim), silent = TRUE)
	if(is.numeric(z[1])) {
		z <- z
	} else {z <- c("count"= NA, "sd.rho" = NA)}
	simulation.count[f] <- z["count"]
	simulation.SDs[f] <- z["sd.rho"]
	print(paste(signif(100*f/n.filo, 2), "% completed.", 
		f, "/", n.filo, "filopodia done." ))
}


mcresults <- data.frame("Name" = colnames(all.move), "Sim.in.10000" = simulation.count, "Sim.proportion" = simulation.count/n.sim, "Sim SD" = simulation.SDs)

mcresults[order(mcresults$Sim.proportion),]


#--------------------------------------------------------------------------------
# Visualise simulation results:

# load datasets with simulations
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toGC/LastWorkspace_MarkovChains.Rdata')
# load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC/LastWorkspace_MarkovChains.Rdata') (not ready for VASP! workspace contam with a different analysis)

dev.new(width = 7, height = 3.5)
matplot(simulation.count, simulation.SDs, type = "p", pch = 16, #log = "x",
	xlab = paste("No. simulations in", n.sim, "where Rho(sim) > Rho(orig)"),
	ylab = "No. SDs from simulation mean")

matplot(log(simulation.count, base = 10), simulation.SDs, type = "p", pch = 16, # log = "x",
	xlab = paste("log(P)"),
	ylab = "No. SDs from simulation mean")
	
mcresults

cluster.full <- GoCluster(ccf.tip.dctm, n.clusters = SearchClusterSpace(ccf.tip.dctm, target.size = target.size)$n.clusters)

# top.cluster vs others: 
# top cluster: 
SearchClusterSpace(ccf.tip.dctm, target.size = target.size)
as.numeric(cluster.full == SearchClusterSpace(ccf.tip.dctm, target.size = target.size)$top.cluster.ID)
TCS.membership <- cluster.full == SearchClusterSpace(ccf.tip.dctm, target.size = target.size)$top.cluster.ID
mcresults$"TCS.member" <- as.numeric(TCS.membership)

mcresults

dev.new(height = 4.6, width = 4.2)
par(mar = c(5,4,1,0)+0.5)
par(bty = "n")
plot(simulation.count, simulation.SDs, type = "p", pch = 18, cex = 1.75,
	# Color by cluster membership:
	col = as.vector(factor(TCS.membership, labels = as.character(curr.cols.60[1:2]))),
	# Color by CCF at 0:
	#col = as.vector(cut(means.at.0, breaks = 11, labels = curr.pal)),
	#log = "x",
	xlab = paste("No. simulations where Rho(sim) > Rho(orig)"),
	ylab = "SD from simulation mean"
	#,cex = 3*abs(means.at.0)
	)
abline(v = 100, col = "grey", lty = 2)
curr.cols.60
means.at.0 <- unlist(ccf.tip.dctm["0", ])

dev.new(width = 6, height = 4.9)

	plot(means.at.0, -log(simulation.count/n.sim, base = 10), pch = 16,
		ylab = "-log(P) ",
		xlab = "CCF at offset 0", ylim = c(0, 4.1)
		#,col = as.vector(factor(TCS.membership, labels = as.character(curr.pal[c(2,9)])))
	)
	points(means.at.0[TCS], -log(simulation.count[TCS]/n.sim, base = 10), pch= 0, cex = 2) 
	abline(h = 0, lty = 2)
	abline(h = -log(0.05, 10), col = "lightgrey", lty = 3)
	abline(h = -log(0.01, 10), col = "lightgrey", lty = 3)
	abline(h = -log(0.001, 10), col = "lightgrey", lty = 3)
	abline(h = -log(0.0001, 10), col = "lightgrey", lty = 3)
	text(par("usr")[1], y = -log(0.05, 10)+0.1, 
		"p = 0.05 (500 simulations)", pos = 4, cex = 0.8, col = "lightgrey")
	text(par("usr")[1], y = -log(0.01, 10)+0.1, 
		"p = 0.01 (100 simulations)", pos = 4, cex = 0.8, col = "lightgrey")
	text(par("usr")[1], y = -log(0.001, 10)+0.1,
		 "p = 0.001 (10 simulations)", pos = 4, cex = 0.8, col = "lightgrey")
	text(par("usr")[1], y = -log(0.0001, 10)+0.1, 
		"p = 0.0001 (1 simulation)", pos = 4, cex = 0.8, col = "lightgrey")
	
#--------------------------------------------------------------------------------
# Visualise simulation method:
rm(list= ls())
load('~/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_ENA/Huang4-01_Norm-toGC/LastWorkspace_MarkovChains.Rdata')

# IMPORTANT: REEXECUTE THE SimulateFilo FUNCTION! 

SimulateFilo <- function(y, x, n.sim = n.sim, output = "count", ...) {
	
	stopifnot(length(x) == length(y))	
	
	#----
	# 1. Prepare required elements and Markov Chain properties:

	y1 <- y[NonNArange(y)]
	x1 <- x[NonNArange(x)]
	
	states.y = c(1:9)
	states.x = c(1:9) # this will need upgrading to handle NAs too 
	y.d <- cut(y1, 9, labels = states.y) # 'd' for discrete
	x.d <- cut(x1, 9, labels = states.x)

	# Solving the NAs problem:
	
	if(sum(is.na(y.d)) > 0) {
		states.y[10] <- NA
	}

	if(sum(is.na(x.d)) > 0) {
		states.x[10] <- NA
	}

	tMatrixY <-  markovchainFit(y.d)$estimate@transitionMatrix
	tMatrixX  <- markovchainFit(x.d)$estimate@transitionMatrix

	mcY <- new("markovchain", 
		states = as.character(colnames(tMatrixY)), 
		byrow = TRUE, 
		transitionMatrix = tMatrixY,
		name = "Movement")

	mcX <- new("markovchain", 
		states = as.character(colnames(tMatrixX)), 
		byrow = TRUE, 
		transitionMatrix = tMatrixX,
		name = "Fluorescence")
	
	#----
	# 2. Generate <n.sim> simulations
		
	sim.rho <- rep(NA, length = n.sim)
	sim.p <- rep(NA, length = n.sim)
	sim.move <- data.frame(matrix(NA, ncol = n.sim, nrow = length(y)-bb))
	sim.tip  <- data.frame(matrix(NA, ncol = n.sim, nrow = length(x)-bb))
	
	orig.rho <- cor.test(as.numeric(y), as.numeric(x), method = "spearman")$estimate
	orig.p <- cor.test(as.numeric(y), as.numeric(x), method = "spearman")$p.value
		
	for (i in 1:n.sim) {
		
		move.sim.i <- as.integer(rmarkovchain(121, object = mcY, t0 = y.d[1]))
		tip.sim.i <- as.integer(rmarkovchain(121, object = mcX, t0 = x.d[1]))	
	
		sim.move[, i] <- move.sim.i
		sim.tip[, i] <- tip.sim.i
		
		p.i <- cor.test(as.numeric(move.sim.i), as.numeric(tip.sim.i), 
				method = "spearman")$p.value
		rho.i <- cor.test(as.numeric(move.sim.i), as.numeric(tip.sim.i), 
				method = "spearman")$estimate
		
		sim.p[i] <- p.i 
		sim.rho[i] <- rho.i 
	}
	
	#----
	# 3. Compare all simulations with orig. x & y

	# 3a. Output how many simulations were as correlated as the original
	
	
		count <- Count(which(sim.rho > orig.rho))
	
	# 3b. output how many SDs away from mean
		sd <- GetSDfromMean(sim.rho, orig.rho)

		print(c("count" = count, "sd" = sd))

	# 3c. output: complete
		if(output == "complete") {
			z <- list()
			z$count = count			
			z$sd = sd
			z$sim.move = sim.move
			z$sim.tip = sim.tip
			z$sim.p = sim.p
			z$sim.rho = sim.rho
			
			return(z)
		} else {	
			return(c("count" = count, "sd" = sd))
			}

}
getwd()

# SAVE TABLE AS CSV FILE:

#setwd(Loc)
#setwd("/Users/Lab/Documents/Postdoc/ANALYSIS_local-files/ANALYSIS LOGS/2017-03_TipF_withBg_VASP/Huang4-01_Norm-toGC")
#write.csv(mcresults[order(mcresults$Sim.proportion), ], file = "MarkovChain_results_VASP.csv")


###------------------------------------------------------------------------
### VISUALISATION OF ONE EXAMPLE FILOPODIUM

# A good candidate filopodium for ENA DATASET:
# 36   DCTM (9)_5-4-01-4            0         0.0000  3.3561655
cf <- 36

# Raw data:
tip.f[, cf]
all.move[, cf]

# Discrete data:
dS.i <- all.dS[, cf]
x.disc <- as.numeric(cut(tip.f[, cf], 9, levels = c(1:9)))
y.disc <- as.numeric(cut(all.move[, cf], 9, levels = c(1:9)))

# Simulation data:
ls()

# recreate an example simulation for this filopodium: 
set.seed(0.1)
cf.results <- SimulateFilo(all.move[, cf], tip.f[, cf], 10000, output = "complete")
cf.results$sim.rho[which.max(cf.results$sim.rho)]
sim.max <- which.max(cf.results$sim.rho)

zzz = 19

dS.i <- all.dS[, cf]
x.disc <- as.numeric(cut(tip.f[, cf], 9, levels = c(1:9)))
y.disc <- as.numeric(cut(all.move[, cf], 9, levels = c(1:9)))
sim.max <- zzz  # chosen arbitrarily; not far from mean (0.06, mean 0.02)

head(cf.results$sim.rho, 100) 

names(cf.results)

cf.sim.move <- cf.results$sim.move [, sim.max]
cf.sim.tip <- cf.results$sim.tip [, sim.max]
cf.dS.i  <- seq(0,240,by = 2)

#---------
# PLOT 1:

# data.frame(dS.i, x.disc, y.disc)

dev.new(width = 8, height = 5.4)
par(mfrow=c(2,2))
par(mar = c(4,4,1,4) + 0.1)
matplot(dS.i, y.disc,
	type = "l",
	lwd = 2,
	col = cols[2],
#	ylim = c(ylo, yhi),
	ylim = c(0, 10),
	xlab = "Time [s]",
	ylab = expression("Tip movement [bin no.]"),
	)
matplot(dS.i, x.disc,
	col = cols[1],
	lwd = 2,
	type="l",
	add = TRUE,
	ylim = c(0, 10)
)
#legend("bottomleft", legend = paste(name), bty = "n")

matplot(x.disc, y.disc,
	 pch = 15,
	 cex = 2.5,
	 col = "#00CC0025",
	 ylim = c(0, 10),
    xlim = c(0, 10),
    xlab = "Observed tip fluorescence [bin no.]",
    ylab = "Observed tip movement [bin no.]"
	 )
curr.rho <- cor(x.disc, y.disc, method = "spearman", use = "pairwise.complete.obs") 
orig.rho <- curr.rho
legend("bottomright", legend = paste("Rho = ", signif(curr.rho, 2)), bty = "n")


# PLOT 3 and 4: redefine x.disc and y.disc, and re-execute code above

x.disc <- cf.sim.tip
y.disc <- cf.sim.move
length(y.disc)

# Now re-execute the code above.

matplot(cf.dS.i, y.disc,
	type = "l",
	lwd = 2,
	col = cols[2],
#	ylim = c(ylo, yhi),
	ylim = c(0, 10),
	xlab = "Time [s]",
	ylab = expression("Tip movement [bin no.]"),
	)
matplot(cf.dS.i, x.disc,
	col = cols[1],
	lwd = 2,
	type="l",
	add = TRUE,
	ylim = c(0, 10)
)
#legend("bottomleft", legend = paste(name), bty = "n")

matplot(x.disc, y.disc,
	 pch = 15,
	 cex = 2.5,
	 col = "#00CC0025",
	 ylim = c(0, 10),
    xlim = c(0, 10),
    xlab = "Observed tip fluorescence [bin no.]",
    ylab = "Observed tip movement [bin no.]"
	 )
curr.rho <- cor(x.disc, y.disc, method = "spearman", use = "pairwise.complete.obs") 
legend("bottomright", legend = paste("Rho = ", signif(curr.rho, 2)), bty = "n")

}


# PLOT 5:  HISTOGRAM for ONE FILOPODIUM

dev.new(width = 8, height = 5.4)
par(mfrow=c(2,2))
par(mar = c(4,4,1,4) + 0.1)

sim.rho <- cf.results$sim.rho

hist(sim.rho,  # breaks = 20,
	main = "",
	xlab = "Spearman Rho",
	ylab = "Frequency",
	col = "darkgrey",
	border = "white"
#	,xlim = x.boundary
	)
abline(v = orig.rho,
	col = "red",
	lty = 2)
# legend("topleft", legend = paste(n.sim, "simulations\n max =", signif(max(sim.rho),2)), text.col = "black", bty = "n")
max(sim.rho)
# legend("bottomright", legend = paste("n(sim) [Rho > orig]\n/n(sim) =", signif(p.rho,2)), bty = "n")
# legend("bottomright", legend = paste("P(Rho) =", signif(p.rho,2)), bty = "n")
# legend("topright", legend = paste("SD from mean:", signif(sd.count, 2)), bty = "n", text.col = "red")	
