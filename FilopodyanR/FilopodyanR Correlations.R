# This script computes correlations between different properties of filopodia in a 
# dataset processed with "FilopodyanR Correlations_DataInput.R" script.

# -----------------------------------------------------------------------------------
# FilopodyanR CORRELATIONS

# Dataset:

# either continue straight from previous script, or clean workspace and continue from saved:

# rm(list = ls())
# load('CorrelationsInput.RData')  # <--- Load the saved .Rdata file from the "Correlations_DataInput" script)


#-----------------------------------------------------------------------------------
# DEPENDENCIES: corrplot

# install.packages('corrplot', dependencies= TRUE, repos='http://cran.rstudio.com/') 
library(corrplot)
# install.packages('psych', dependencies= TRUE, repos='http://cran.rstudio.com/') 
library(psych)


#-----------------------------
# LENGTH AND MOVEMENT METRICS:

# MAX LENGTH:		  	  :max.lengths 			Max length reached:  max.lengths
# MEAN LENGTH:			  :length.mean 			Mean length
# STRAIGHTNESS AT MAX: straightness.at.max.over5 (max length > 5, otherwise straightness measurements not linear; see xy plot)
# TIP SPEED EXT:	  	  :med.rate.extens   	Median tip extending speed (over threshold, fdctm > threshold.ext.per.t)
# TIP SPEED RETR: 		  :med.rate.retract 	Median tip retracting speed (below thresh, fdctm < threshold.retr.per.t)
# BASE SPEED INVAS:		  :med.fdcbm.invas  		Median base invading speed (fdcbm > threshold.ext.per.t) 
												# code change on 28.8.16 from (dcbm > 0)
# BASE SPEED RETR: 		  :med.fdcbm.retract     Median base retracting speed (fdcbm < thresh)
# INITIAL TIP SPEED:	  :med.fdctm.initial  	Median tip speed during 0-20 s; # recoded to use fdctm (not dctm99)
# INITIAL BASE SPEED:  	  :med.fdcbm.initial	Median base speed during 0-20 s; # recoded to use fdcbm
# TIP PERSISTENCE(raw)	  :acf.dctm.roots		
#	 			 (fdctm)  :acf.fdctm.roots

# TIME SPENT EXTENDING:	  :all.time.ext
# TIME SPENT RETRACTING:  :all.time.retr	
# TIME SPENT STALLING:    :all.time.stall


#-----------------------------
# FLUORESCENCE METRICS:

# BASE PRE-FORMATION:   : mean.base.nor.1t 
#						: mean.base.nor.3t
#						: mean.base.nor.5t
#						: mean.base.nor.10t

# TIP INTIIAL: 			: mean.tip.nor.1t
#						: mean.tip.nor.3t
#						: mean.tip.nor.5t
#						: mean.tip.nor.10t
# TIP INTIIAL (th):		: mean.tip.th.1t
#						: mean.tip.th.3t
#						: mean.tip.th.5t
#						: mean.tip.th.10t



# CALIBRATE TO spt ALL THE PARAMETERS USED DOWNSTREAM:

med.rate.extens   <- med.rate.extens / spt
med.rate.retract  <- med.rate.retract / spt
med.fdcbm.insas   <- med.fdcbm.invas / spt
med.fdcbm.retract <- med.fdcbm.retract / spt
acf.fdctm.roots   <- acf.fdctm.roots * spt

med.fdctm.initial         <- med.fdctm.initial / spt
med.fdcbm.initial         <- med.fdcbm.initial / spt
med.rate.extens.post10    <- med.rate.extens.post10 / spt
med.rate.retract.post10   <- med.rate.retract.post10 / spt
med.fdcbm.invas.post10    <- med.fdcbm.invas.post10 / spt
med.fdcbm.retract.post10  <-  med.fdcbm.retract.post10 / spt


df.sel.filo.metrics <- data.frame(
	max.lengths,
	straightness.at.max.over5,
#	mean.straightness,	
	med.rate.extens, 
	med.rate.retract, 
	med.fdcbm.invas,  
	med.fdcbm.retract,
#	med.fdB.invas,
#	med.fdB.retract,
#	med.fdctm.initial,
	#	med.fdcbm.initial,	
#	med.fdB.initial,
#	med.fdcbm.initial,
	acf.fdctm.roots,
	all.time.ext,
	all.time.stall,
	all.time.retr
#	,mean.base.nor.3t,
#	mean.tip.nor.3t
)

df.sel.filo.metrics.post10 <- data.frame(
    med.fdctm.initial,
  med.fdcbm.initial,	
  max.lengths,
  med.rate.extens.post10, 
  med.rate.retract.post10, 
  med.fdcbm.invas.post10,  
  med.fdcbm.retract.post10,
  #	med.fdB.invas,
  #	med.fdB.retract,
  #	med.fdB.initial,
  #	med.fdcbm.initial,
  acf.fdctm.roots * spt,
  all.time.ext.post10,
  all.time.stall.post10,
  all.time.retr.post10
  #	,mean.base.nor.3t,
  #	mean.tip.nor.3t
)

var.names <- c(
"Length max",
"Straightness at max",
"Tip Extens med",
"Tip Retract med",
"Base Invas med",
"Base Retract med",
#"Initial Tip Move med",
#"Initial Base Move med",
"Tip Persistence",
"Time Extending",
"Time Stalling",
"Time Retracting"
#,"Base Fl. pre-initiation (last 3t)",
#"Tip Fl. (first 3t)"
)
var.names.post10 <- c(
   "Initial Tip Move med",
  "Initial Base Move med",
  "Length max",
  "Tip Extens med (post10)",
  "Tip Retract med (post10)",
  "Base Invas med",
  "Base Retract med",
  "Tip Persistence",
  "Time Extending (post10)",
  "Time Stalling (post10)",
  "Time Retracting (post10)"
  #,"Base Fl. pre-initiation (last 3t)",
  #"Tip Fl. (first 3t)"
)


var.names.short <- c(
"Length",
"Straight",
"Tip ext",
"Tip retr",
"Base invas",
"Base retr",
#"Initial Tip Move med",
#"Initial Base Move med",
"Tip Persist",
"Time Ext",
"Time Stall",
"Time Retr"
#,"Base Fl. pre-initiation (last 3t)",
#"Tip Fl. (first 3t)"
)
var.names.post10.short <- c(
  "Initial DCTM",
  "Initial DCBM",
  "Length",
  "Tip ext",
  "Tip retr",
  "Base invas",
  "Base retract",
  "Tip persist",
  "Time Ext",
  "Time Stall",
  "Time Retr"
  #,"Base Fl. pre-initiation (last 3t)",
  #"Tip Fl. (first 3t)"
)

ncol(df.sel.filo.metrics)
# colnames(df.sel.filo.metrics) <- var.names
# colnames(df.sel.filo.metrics) <- var.names.short
colnames(df.sel.filo.metrics.post10) <- var.names.post10
colnames(df.sel.filo.metrics.post10) <- var.names.post10.short
df.sel.filo.metrics


# Which corr method?
corr.matrix <- cor(df.sel.filo.metrics, use = "pairwise.complete.obs", method = "spearman")
# corr.matrix <- cor(df.sel.filo.metrics, use = "pairwise.complete.obs", method = "spearman")
corr.matrix.post10 <- cor(df.sel.filo.metrics.post10, use = "pairwise.complete.obs", method = "spearman")
corr.matrix
corr.matrix.post10

correlation.list <- corr.test(df.sel.filo.metrics, use = "pairwise.complete.obs", method = "spearman", adjust = "holm")
str(correlation.list)
correlation.p <- corr.test(df.sel.filo.metrics, use = "pairwise.complete.obs", method = "spearman", adjust = "holm")$p
correlation.rho <- corr.test(df.sel.filo.metrics, use = "pairwise.complete.obs", method = "spearman", adjust = "holm")$r

write.csv(correlation.p, "correlation-p-values.csv")
write.csv(correlation.rho, "correlation-rho-values.csv")

correlation.list2 <- corr.test(df.sel.filo.metrics.post10, use = "pairwise.complete.obs", method = "spearman")
correlation2.p <- correlation.list2$p
correlation2.rho <- correlation.list2$r

write.csv(correlation2.p, "correlation-post10_p-values.csv")
write.csv(correlation2.rho, "correlation-post10_rho-values.csv")


# Corr. matrix  - overall

dev.new()
corrplot(corr.matrix, method = "circle", type = "upper", diag = FALSE)
corrplot(corr.matrix.post10, method = "circle", type = "upper", diag = FALSE)

dev.new()
corrplot.mixed(corr.matrix, upper = "circle", lower = "number")
corrplot.mixed(corr.matrix.post10, upper = "circle", lower = "number", )

# Col2 = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")


#-----------------------------------------------------------------------------------
# XY Plots

XY.plot <- function(x, y, legend.where = "bottomright", corr.method = "pearson", ...) {
  
  matplot(x, y, type = "p",
          pch = 16,
          col = "#00000075",
          ...)
  rho <- cor.test(x,y, method = corr.method)$estimate
 # p <- cor.test(x,y, method = "spearman")$p.value
    
  if(corr.method == "pearson") {
  		method.as.text = "Pearson R" 
  } else if (corr.method == "spearman")  {
  		method.as.text = "Spearman Rho"			
  }
  
  legend(legend.where, bty = "n",
  	legend = paste(method.as.text, "=", signif(rho, 3)) 
  	)
  	
#  legend(legend.where, legend = paste("Spearman Rho =", signif(rho, 3)), bty = "n")  
}

DrawRegLine <- function(x, fit, ...) {
#	x1 <- par("usr")[1]
#	x2 <- par("usr")[1]
	x1 <- min(x, na.rm = TRUE)
	x2 <- max(x, na.rm = TRUE)	
	
	slope <- fit$"coefficients"[2]
	int <- fit$"coefficients"[1]
	
	y1 <- x1*slope + int
	y2 <- x2*slope + int
	
	arrows(x1, y1, x2, y2, length = 0, code = 3, ...)
}

DrawPredictedCI <- function(x, x.colname, fit, ...) {
	newx <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length(x))	
	newdata <- data.frame(newx); colnames(newdata) <- x.colname
	pred <- predict(fit, newdata, interval = 'confidence', level = 0.95)
	polygon(c(rev(newx), newx), c(rev(pred[ ,3]), pred[ ,2]), border = NA, ...)
}

#-----------------------------------------------------------------------------------

dev.new(width = 4, height = 3)
par(mar = c(4,4,1,1)+0.1)

# A) Persistence vs Length

XY.plot(acf.fdctm.roots, max.lengths, 
		legend.where = "topright",
        ylab = expression("Max length [" * mu * "m]"),
        xlab = "Tip persistence [s]"
        , corr.method = "spearman"
        )
cor.test(max.lengths, acf.fdctm.roots, method = "spearman")
cor.test(max.lengths, acf.fdctm.roots, method = "pearson")

colnames(df.sel.filo.metrics)
fit1 <- lm(formula = max.lengths ~ acf.fdctm.roots, data = df.sel.filo.metrics)

DrawPredictedCI(acf.fdctm.roots, "acf.fdctm.roots", fit1, col = paste0(Col2[9], "30"))
DrawRegLine(acf.fdctm.roots, fit1, col = Col2[9], lwd = 3)


# B) Extens rate vs Length

XY.plot(med.rate.extens, max.lengths, 
		legend.where = "topright",
        xlab = expression("Tip extension med [" * mu * "m/s]" ),
        ylab = expression("Max length [" * mu * "m]"),
        corr.method = "spearman"
        )
cor.test(med.rate.extens, max.lengths, method = "spearman")
#cor.test(med.rate.extens, max.lengths, method = "pearson")

# C) Extens rate vs Time extending

XY.plot(med.rate.extens, all.time.ext, 
		legend.where = "bottomright",
        xlab = expression("Tip extension med [" * mu * "m/s]" ),
        ylab = "Time extending",
        corr.method = "spearman"
)
cor.test(med.rate.extens, all.time.ext, method = "spearman")
#cor.test(med.rate.extens, all.time.ext, method = "pearson")

# D) Extens rate vs Time stalling

XY.plot(med.rate.extens, all.time.stall, 
		legend.where = "topright",
        xlab = expression("Tip extension med [" * mu * "m/s]" ),
        ylab = "Time stalling",
        corr.method = "spearman"
        )
cor.test(med.rate.extens, all.time.stall, method = "spearman")
#cor.test(med.rate.extens, all.time.stall, method = "pearson")

fit4 <- lm(formula = all.time.stall ~ med.rate.extens, data = df.sel.filo.metrics)

DrawPredictedCI(med.rate.extens, "med.rate.extens", fit4, col = paste0(Col2[2], "30"))
DrawRegLine(med.rate.extens, fit4, col = Col2[2], lwd = 3)


#-----------------------------------------------------------------------------------
# XY plots for initial DCTM:

# df.sel.filo.metrics.post10 <- data.frame(
    # med.fdctm.initial,
  # med.fdcbm.initial,	
  # max.lengths,
  # med.rate.extens.post10, 
  # med.rate.retract.post10, 
  # med.fdcbm.invas.post10,  
  # med.fdcbm.retract.post10,
  # #	med.fdB.invas,
  # #	med.fdB.retract,

  # #	med.fdB.initial,
  # #	med.fdcbm.initial,
  # # 	initial.fdctm.ratio1, 	# Added: 30 August 2016
  # #	initial.fdctm.ratio2,
  # acf.fdctm.roots,
  # all.time.ext.post10,
  # all.time.stall.post10,
  # all.time.retr.post10
  # #	,mean.base.nor.3t,
  # #	mean.tip.nor.3t
# )

# dev.new(width = 4, height = 3)
# par(mar = c(4,4,1,1)+0.1)


# XY.plot(med.fdctm.initial/2, med.rate.extens.post10/2, 
		# legend.where = "topleft",
        # xlab = expression("Initial tip extension [" * mu * "m/s]" ),
        # ylab = expression("Tip extension [" * mu * "m/s]" ),
        # corr.method = "spearman"
        # )
# cor.test(med.fdctm.initial/2, med.rate.extens.post10, method = "spearman")

# XY.plot(med.fdctm.initial/2, max.lengths, 
		# legend.where = "topleft",
        # xlab = expression("Initial tip extension [" * mu * "m/s]" ),
        # ylab = expression("Max length [" * mu * "m]" ),
        # corr.method = "spearman"
        # )
# cor.test(med.fdctm.initial/2, max.lengths, method = "spearman")


# XY.plot(med.fdctm.initial/2, all.time.stall.post10, 
		# legend.where = "topright",
        # xlab = expression("Initial tip extension [" * mu * "m/s]" ),
        # ylab = expression("Time stalling" ),
        # corr.method = "spearman"
        # )
# cor.test(med.fdctm.initial/2, all.time.stall.post10, method = "spearman")


# XY.plot(med.fdctm.initial/2, all.time.ext.post10, 
		# legend.where = "topleft",
        # xlab = expression("Initial tip extension [" * mu * "m/s]" ),
        # ylab = expression("Time extending" ),
        # corr.method = "spearman"
        # )
# cor.test(med.fdctm.initial/2, all.time.ext.post10, method = "spearman")

#-----------------------------------------------------------------------------------
# Histograms:

dev.new(width = 4, height = 5)
par(mfrow = c(3,2))
par(mar = c(4,4,1,1)+0.1)

Hist <- function(x, xlab, ...) {
	z <- hist(x, plot = FALSE, ...)
	z$counts <- z$counts / sum(z$counts)
	plot(z, main = "", 
		col = "grey",
		border = "white",
		xlab = xlab,
		...)	
}
par(bty = "l")


Hist(max.lengths, 
	xlab = expression("Max length [" * mu * "m]"), ylab = "Probability density" 
	,breaks = 12)
	box()
Hist(straightness.at.max.over5, 
	xlab = "Straightness [au]", ylab = "" 
	,breaks = 25)
	box()
Hist(med.rate.extens, 
	xlab = expression("Tip extension rate [" * mu * "m/s]"), ylab = "Probability density" 	
	,xlim = c(0.015, 0.2) 	#,breaks  = 12
	)
	box()
Hist(med.fdcbm.invas, 
	xlab = expression("Base invasion rate [" * mu * "m/s]"), ylab = "",
	xlim = c(0.015, 0.6) 
	#,breaks  = 12
	)
	box()
Hist(all.time.ext,
 	xlab = "Time extending [fraction of total]", ylab = "Probability density" 
 	#,breaks = 12
 	)
 	box()
Hist(acf.fdctm.roots,
 	xlab = "Tip persistence [s]", ylab = "" 
 	#,breaks = 12
 	)
 	box()


# sanity check:

cor(max.lengths, med.rate.extens, use = "pairwise.complete.obs")
data.frame(max.lengths, med.rate.extens)
data.frame(max.lengths[new.from.short], med.rate.extens[new.from.short])
cor(max.lengths[new.from.short], med.rate.extens[new.from.short], use = "pairwise.complete.obs")

# Tip Base Fluorescence Correlations:

df.sel.fl.metrics <- data.frame(mean.base.nor.10t, mean.base.nor.5t, mean.base.nor.3t, mean.base.nor.1t, mean.tip.nor.1t,  mean.tip.nor.3t, mean.tip.nor.5t, mean.tip.nor.10t)

corr.matrix.fl <- cor(df.sel.fl.metrics, use = "pairwise.complete.obs")
dev.new()
corrplot(corr.matrix.fl, method = "circle")


# Base Fluorescence (various t) compared to all

df.base.to.all <- data.frame(mean.base.nor.10t, mean.base.nor.5t, mean.base.nor.3t, mean.base.nor.1t, max.lengths, med.rate.extens, med.rate.retract, med.fdctm.initial, all.time.ext, all.time.stall, all.time.retr)
df.base.to.all2 <- data.frame(mean.base.nor.10t, mean.base.nor.5t, mean.base.nor.3t, mean.base.nor.1t, max.lengths, med.rate.extens, med.rate.retract, med.fdcbm.invas, med.fdcbm.retract, med.fdctm.initial, med.fdcbm.initial, all.time.ext, all.time.stall, all.time.retr)

corr.matrix.base.to.all <- cor(df.base.to.all, use = "pairwise.complete.obs")
corr.matrix.base.to.all2 <- cor(df.base.to.all2, use = "pairwise.complete.obs")
dev.new()
corrplot(corr.matrix.base.to.all)
dev.new()
corrplot(corr.matrix.base.to.all2)  # for BaseF_extended.pdf plot


# Tip Fluorescence (various t) compared to all

df.tipF.to.all <- data.frame(mean.tip.nor.1t,  mean.tip.nor.3t, mean.tip.nor.5t, mean.tip.nor.10t, max.lengths, med.rate.extens, med.rate.retract, med.fdctm.initial, all.time.ext, all.time.stall, all.time.retr)
df.tipF.to.all2 <- data.frame(mean.tip.nor.1t,  mean.tip.nor.3t, mean.tip.nor.5t, mean.tip.nor.10t, max.lengths, med.rate.extens, med.rate.retract, med.fdcbm.invas, med.fdcbm.retract, med.fdctm.initial, all.time.ext, all.time.stall, all.time.retr)

corr.matrix.tipF.to.all <- cor(df.tipF.to.all, use = "pairwise.complete.obs")
corr.matrix.tipF.to.all2 <- cor(df.tipF.to.all2, use = "pairwise.complete.obs")
dev.new()
corrplot(corr.matrix.tipF.to.all)
corrplot(corr.matrix.tipF.to.all2)


# Is this above robust? Take Th Tip instead of tip

df.tipF.to.all <- data.frame(mean.tip.th.1t,  mean.tip.th.3t, mean.tip.th.5t, mean.tip.th.10t, max.lengths, med.rate.extens, med.rate.retract, med.fdctm.initial, all.time.ext, all.time.stall, all.time.retr)

corr.matrix.tipF.to.all <- cor(df.tipF.to.all, use = "pairwise.complete.obs")
dev.new()
corrplot(corr.matrix.tipF.to.all)

cor(max.lengths, med.rate.extens, use = "pairwise.complete.obs", method = "spearman") # 0.4045236
cor(max.lengths, acf.fdctm.roots, use = "pairwise.complete.obs", method = "spearman") # 0.490414
cor(max.lengths, acf.dctm.roots, use = "pairwise.complete.obs", method = "spearman") # 0.4848335

cor.test(max.lengths, med.rate.extens, method = "spearman")  # 


# cor.test(max.lengths, med.rate.extens, method = "pearson"); # cor = 0.1761853, p-value = 0.03161
# cor.test(max.lengths, acf.fdctm.roots, method = "pearson"); # cor = 0.5254087, p-value = 7.105e-15
cor.test(max.lengths, med.rate.extens, method = "spearman"); # S = 167230, p-value = 5.047e-06, rho = 0.4045236 
cor.test(max.lengths, acf.fdctm.roots, method = "spearman"); # S = 269770, p-value = 2.878e-10; rho = 0.490414 
cor.test(med.rate.extens, all.time.ext, method = "spearman"); # S = 135260, p-value = 1.555e-09; rho = 0.5183789 
cor.test(med.rate.extens, all.time.stall, method = "spearman"); # S = 432240, p-value = 2.53e-10; rho = -0.5390923

Count(max.lengths)
Count(acf.fdctm.roots)
Count(med.rate.extens)
Count(all.time.ext)
Count(all.time.stall)

# df.sel.filo.metrics: # 
#	max.lengths,
#	med.rate.extens, 
#	med.rate.retract, 
#	med.fdcbm.invas,
#	med.fdcbm.retract,	
#	med.fdctm.initial,
#	med.fdcbm.initial,
# 	initial.fdctm.ratio1, 	# Added: 30 August 2016
#	initial.fdctm.ratio2,
#	acf.fdctm.roots,
#	all.time.ext,
#	all.time.stall,
#	all.time.retr,
#	mean.base.nor.3t,
#	mean.tip.nor.3t)

# cor.test(med.fdctm.initial, all.time.ext, method = "pearson", use = "pairwise.complete.obs")
# cor.test(med.fdctm.initial, all.time.retr, method = "pearson", use = "pairwise.complete.obs")
# cor.test(med.fdctm.initial, all.time.stall, method = "pearson", use = "pairwise.complete.obs")
# THESE ARE NOT INDEPENDENT! (Stalling/extension/retr status defined through its fdctm, 
# i.e. for the first 10 timepoints, the initial fdctm will directly determine tip status. Therefore,
# need to exclude first 10 timepoints --> Use the .post10 data.)

timecounts <- apply(all.dT, 2, function(x) Count(x))
at.least.30t <- which(timecounts > 30)

# # df.sel.filo.metrics.post10 <- data.frame(
    # med.fdctm.initial,
  # med.fdcbm.initial,	
  # max.lengths,
  # med.rate.extens.post10, 
  # med.rate.retract.post10, 
  # med.fdcbm.invas.post10,  
  # med.fdcbm.retract.post10,
  # #	med.fdB.invas,
  # #	med.fdB.retract,

  # #	med.fdB.initial,
  # #	med.fdcbm.initial,
  # # 	initial.fdctm.ratio1, 	# Added: 30 August 2016
  # #	initial.fdctm.ratio2,
  # acf.fdctm.roots,
  # all.time.ext.post10,
  # all.time.stall.post10,
  # all.time.retr.post10
  # #	,mean.base.nor.3t,
  # #	mean.tip.nor.3t
# )

# Correlations between initial tip movement (within first 10 timepoints) and other parameters (after first 10 timepoints):

cor.test(med.fdctm.initial, all.time.ext.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdctm.initial, all.time.retr.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdctm.initial, all.time.stall.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdctm.initial, med.rate.extens.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdctm.initial, med.rate.retract.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdctm.initial, max.lengths, method = "spearman", use = "pairwise.complete.obs")

cor.test(med.fdcbm.initial, all.time.ext.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdcbm.initial, all.time.retr.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdcbm.initial, all.time.stall.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdcbm.initial, med.rate.extens.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdcbm.initial, med.rate.retract.post10, method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdcbm.initial, max.lengths, method = "spearman", use = "pairwise.complete.obs")


# Correlations between initial base movement (within first 10 timepoints) and other parameters (after first 10 timepoints):

cor.test(med.fdctm.initial[at.least.30t], all.time.ext.post10[at.least.30t], 
	method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdctm.initial[at.least.30t], all.time.retr.post10[at.least.30t], 
	method = "spearman", use = "pairwise.complete.obs")
cor.test(med.fdctm.initial[at.least.30t], all.time.stall.post10[at.least.30t], 
	method = "spearman", use = "pairwise.complete.obs")


dev.new()
par(mfrow = c(2,2))
matplot(med.fdctm.initial[at.least.30t], all.time.ext[at.least.30t],
	main = "",
	pch = 4,
	cex = 0.4,
	xlab = expression("fDCTM [" * mu * "m / 2s]"),
	ylab = expression("Time extending ")
)
matplot(med.fdctm.initial[at.least.30t], all.time.ext.post10[at.least.30t],
	main = "",
	pch = 4,
	cex = 0.4,
	xlab = expression("fDCTM [" * mu * "m / 2s]"),
	ylab = expression("Time extending ")
)
matplot(med.fdctm.initial[at.least.30t], all.time.stall[at.least.30t],
	main = "",
	pch = 4,
	cex = 0.4,
	xlab = expression("fDCTM [" * mu * "m / 2s]"),
	ylab = expression("Time stalling")
)
matplot(med.fdctm.initial[at.least.30t], all.time.stall.post10[at.least.30t],
	main = "",
	pch = 4,
	cex = 0.4,
	xlab = expression("fDCTM [" * mu * "m / 2s]"),
	ylab = expression("Time stalling")
)

dev.new()
par(mfrow = c(2,2))
matplot(all.time.ext[at.least.30t], all.time.ext.post10[at.least.30t],
	main = "",
	pch = 4,
	cex = 0.4
	#xlab = expression("fDCTM [" * mu * "m / 2s]"),
	#ylab = expression("Time extending ")
)
matplot(all.time.ext[at.least.30t], all.time.ext.post10[at.least.30t],
	main = "",
	pch = 4,
	cex = 0.4
	#xlab = expression("fDCTM [" * mu * "m / 2s]"),
	#ylab = expression("Time extending ")
)
matplot(all.time.stall[at.least.30t], all.time.stall.post10[at.least.30t],
	main = "",
	pch = 4,
	cex = 0.4
	#xlab = expression("fDCTM [" * mu * "m / 2s]"),
	#ylab = expression("Time extending ")
)



Count(med.rate.extens)
Count(acf.fdctm.roots)

# n numbers

ncol(all.length)
Count(new)
Count(new.from.short)

Count(max.lengths)
Count(med.rate.extens)
Count(med.rate.retract)
Count(med.fdctm.initial)
Count(all.time.ext)
Count(all.time.stall)
Count(all.time.retr)

Count(mean.tip.nor.1t)
Count(mean.tip.nor.3t)
Count(mean.tip.nor.5t)
Count(mean.tip.nor.10t)

Count(mean.base.nor.1t)
Count(mean.base.nor.3t)
Count(mean.base.nor.5t)
Count(mean.base.nor.10t)


# dev.new()
# dev.new()
# par(mfrow = c(2,2))
# matplot(max.lengths, med.rate.extens,
	# main = "Max Length vs Tip extension rate",
	# pch = 4,
	# cex = 0.4,
	# xlab = expression("Max Length [" * mu * "m]"),
	# ylab = expression("fDCTM [" * mu * "m / 2s]"),
# )

# matplot(max.lengths, acf.fdctm.roots,
	# main = "Max Length vs Tip persistence",
	# pch = 4,
	# cex = 0.4,
	# xlab = expression("Max Length [" * mu * "m]"),
	# ylab = expression("Root of ACF fDCTM [" * mu * "m / 2s]"),
# )

# matplot(max.lengths, med.fdctm.initial,
	# main = "Max Length vs Initial Tip Movement",
	# pch = 4,
	# cex = 0.4,
	# xlab = expression("Max Length [" * mu * "m]"),
	# ylab = expression("Initial fDCTM [" * mu * "m / 2s]"),
# )

# matplot(all.time.ext, med.fdctm.initial,
	# main = "Time Extending vs Initial tip movement",
	# pch = 4,
	# cex = 0.4,
	# xlab = expression("% Time extending"),
	# ylab = expression("Initial fDCTM [" * mu * "m / 2s]")
# )


help(cor.mtest)
library(Hmisc)
rcorr(corr.matrix, type = "spearman")