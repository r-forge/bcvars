# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p3Calc <- function(p2, p7, outfile, format='') {
	
	if (trim(class(p2)) != "RasterLayer") {
		stop('First element should be a RasterLayer [bio_2]')
	} else if (trim(class(p7)) != "RasterLayer") {
		stop('Second element should be a RasterLayer [bio_7]')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Isothermality (P3)", "\n")
			b3fun <- function(x,y) {return(round(100 * x / y))}
			p3 <- overlay(p2, p7, fun=b3fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
		} else {
			cat("", "\n", "File bio_3 already exists, skipping calculation, but loading", "\n")
			p3 <- raster(outfile)
		}
		return(p3)
}
