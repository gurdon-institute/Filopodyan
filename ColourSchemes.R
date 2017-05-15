# Plot templates:

# Colour schemes

# rm(list = setdiff(ls(), keep))

# dual - cadet-red
dual1.light <- data.frame("c" = c(64, 127, 127), "r" = c(212, 106, 106)) / 255
dual1.dark  <- data.frame("c" = c(13, 77, 77), "r" = c(128, 21, 21)) / 255
dual1.mixed <- data.frame("c" = c(64, 127, 127), "r" = c(128, 21, 21)) / 255


# dual2 - midnightblue-coral
dual2 <- data.frame("m" = c(25, 25, 112), "c" = c(243, 115, 112)) / 255



dual1.light[, 1]

# triadic1 - rainbow
rgb.triad1 <- data.frame("g" = c(0, 157, 125), "o" = c(247, 172, 0), "v" = c(125, 037, 128))
triad1     <- rgb.triad1/255
triad1

# triadic2 - teal-mulberry
rgb.triad2 <- data.frame("t" = c(71, 142, 117), "m" = c(172, 86, 133), "y" = c(212, 202, 106))
triad2     <- rgb.triad2/255
triad2

colourscheme.names <- setdiff(ls(), keep)







cat("Thank you. Colour schemes succesfully imported.")

