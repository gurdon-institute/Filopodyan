# GRAPHING TEMPLATES:

# ------------------------------------------------------------------------------

# Two (or more) boxplots (with jitter)

	# applied to:
	# Max lengths
	# Median extension rate	:
	# Median retraction rate:
	# Growth consistency 	: 	(acf.dctm.crosspoints)
	# Initial DCTM (new filo)
	# Initial dB (new filo)
	# Initial DCTM/dB (new filo)
	# Mean straightness

# Plain histogram 
# Histogram with horizontal boxplot:
# Two overlapping histograms (with transparency)
# CDFs 
# Scatterplots

# ------------------------------------------------------------------------------
# Import colour schemes (internal)

setwd(Loc.Modules)
source("ColourSchemes.R")
print(colourscheme.names)


# ------------------------------------------------------------------------------

# TWO BOXPLOTS (with jitter)

	# applied to:
	# Max lengths
	# Median extension rate	:
	# Median retraction rate:
	# Growth consistency 	: 	(acf.dctm.crosspoints)
	# Initial DCTM (new filo)
	# Initial dB (new filo)
	# Initial DCTM/dB (new filo)
	# Mean straightness



# 1. Standardise data input for graphs:


# Choose among colourscheme.names (from ColourSchemes.R)
curr.cols = triad1  # used for manual-v-batch
curr.cols = triad1[, 3:2]  # used for huang-renyi
curr.cols


print(noquote("Importing graphing functions..."))

StandardGraphInput <- function(x, adjust.spt = "none", flip = "FALSE") {
    
    # 1. Extract data from metalist
    x = as.character(x)
    parameter.to.plot <- which(objectnames[[1]] == x)
    data1 = metalist[[1]][parameter.to.plot] 
    data2 = metalist[[2]][parameter.to.plot] 
    
    stopifnot(exists("spt"))
    
    if(adjust.spt == "multiply") {
    	adj = metalist[[1]]$spt
    } else if(adjust.spt == "divide") {
    	adj = 1 / metalist[[1]]$spt
    } else if (adjust.spt == "none") {
    	adj = 1
    }
    
    if(flip == TRUE) {adj = -adj}
        
    # 2. Organise into table for plotting (curr.data)
    n1 = length(unlist(data1)) 
    n2 = length(unlist(data2))
    
    curr.data = data.frame(
        "Value" = c(unlist(data1) * adj, unlist(data2) * adj), 
        "Source" = c(rep(dataset.names[1], n1), rep(dataset.names[2], n2)))
        # fix alphabetical ordering of X axis, to actual order of dataset names:
        curr.data$Source <- factor(curr.data$Source, levels = dataset.names) 
    
    return(curr.data)
} 




# 2. Quick stats - returns p value for t-test (if large and normally distributed sample) or mann-whitney (otherwise)
#    function call: QuickStats("waviness.mean")

QuickStats <- function(x) {
 
  # 1. Extract data from metalist
    x = as.character(x)
    parameter.to.plot <- which(objectnames[[1]] == x)
    data1 = metalist[[1]][parameter.to.plot] 
    data2 = metalist[[2]][parameter.to.plot] 
    n1 = length(unlist(data1)) 
    n2 = length(unlist(data2))
      
  # 2. Large sample size?
	n.over.30 <- (n1 > 30) & (n2 < 30)
	
  # 3. For big samples:
    if (n.over.30 == TRUE) {
        
       # Test for normal distribution:
        shapiro1 <- shapiro.test(unlist(data1))$p.value
        shapiro2 <- shapiro.test(unlist(data2))$p.value
        appr.normal <- (shapiro1 > 0.1) & (shapiro2 > 0.1)
        
        # Conduct t-test if distr roughly normal
        if (appr.normal == TRUE) {
            t <- t.test(unlist(data1), unlist(data2))$p.value
            t2 <- signif(t, 2)
            z <- c("T-test", paste("p = ", t))
        } 
    
  # For small samples, and large but non-normally distributed samples       
    } else {
        mw <- wilcox.test(Value ~ Source, data = curr.data)$p.value
        mw2 <- signif(mw, 2)  # significant digits
        z <- c("Mann-Whitney", paste("p = ", mw2))
    }  
    return(z)
}



# 2. Create boxplot

Boxplot2 <- function(x, curr.title, curr.Ylab, col = rgb(t(curr.cols))) {

    #dev.new()
    		ylo = min(curr.data$Value, na.rm = TRUE)
            yhi = 1.1 * max(curr.data$Value, na.rm = TRUE)
        boxplot(Value ~ Source, data = curr.data,
            outpch = NA,
            col = col,
            main = curr.title,
            ylab = curr.Ylab,
            ylim = c(ylo, yhi)
            )          
        stripchart(Value ~ Source, data = curr.data,
            add = TRUE,
            vertical = TRUE,
            method = "jitter",
            pch = 4, cex = 0.5,
            col = rgb(0,0,0,0.4)   
            ) 
                       
}

print(noquote("Thank you. Graphing functions imported."))



# ------------------------------------------------------------------------------
# CDFs 

# same examples as boxplot above:

CdfPlot2 <- function(x, adjust.spt = "none", flip = FALSE, 
					curr.title, curr.Xlab, legend.where = "bottomright", 
					legend.make = TRUE, ...) {
	
    if(adjust.spt == "multiply") {
    	adj = metalist[[1]]$spt
    } else if(adjust.spt == "divide") {
    	adj = 1 / metalist[[1]]$spt
    } else if (adjust.spt == "none") {
    	adj = 1
    }
    
    if(flip == TRUE) {adj = -adj}
	
	
	# Extract data
    x = as.character(x)
    parameter.to.plot <- which(objectnames[[1]] == x)
    data1 = unlist(metalist[[1]][parameter.to.plot]) * adj
    data2 = unlist(metalist[[2]][parameter.to.plot]) * adj
    n1 =  Count((data1)) 
    n2 = Count((data2))
	
	# dev.new()
		hilo = range(c(data1, data2), na.rm = TRUE)
		legendstrings = paste(dataset.names, " (n = ", c(n1, n2), ")", sep = "")
	
		
	my.median <- function(x) {median(unlist(x), na.rm = TRUE)}
	curr.medians <- c(median(data1, na.rm = TRUE), median(data2, na.rm = TRUE))
		
	plot(ecdf(data1), 
		main = curr.title,
		xlab = curr.Xlab,
		xlim = hilo,
		cex = 0,
		lwd = 3,
		verticals = TRUE,
		col = rgb(t(curr.cols))[1],
		...
		)
	plot(ecdf(data2), add = TRUE, 
		cex = 0,
		lwd = 3,
		verticals = TRUE,
		col = rgb(t(curr.cols))[2]
		)
	if(legend.make == TRUE) {
		legend(legend.where, 
			legend = legendstrings,
			cex = 1,
			bty = "n",
			lwd = 3,
			col = rgb(t(curr.cols))
			)
		}
		
	arrows(curr.medians[2], 0.5, curr.medians[2], 0.0, 
		length = 0.0, code = 3, angle = 180, col = rgb(t(curr.cols[2])), lty = 1)
	arrows(curr.medians[1], 0.5, curr.medians[1], 0.0, 
		length = 0.0, code = 3, angle = 180, col = rgb(t(curr.cols[1])), lty = 1)
	arrows(hilo[1], 0.5, curr.medians[1], 0.5, 
		length = 0.0, code = 3, angle = 180, col = "lightgrey", lty = 2)
}

# example: 
# CdfPlot2("max.lengths", "Max Filopodium Length (CDF)", expression ("Length [" * mu * "m]"))



# ------------------------------------------------------------------------------

# Big ACF Plot:


Count <- function(x) length(x[!is.na(x)])			 
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(Count(x))	 							
#CI.z <- function(x) 1.96*sd(x, na.rm=TRUE)/sqrt(Count(x))    # Using Z distribution 

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



# ------------------------------------------------------------------------------
