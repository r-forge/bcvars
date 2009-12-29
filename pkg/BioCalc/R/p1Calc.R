# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p1Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (tmean)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Annual Mean Temperature (P1)", "\n")
			b1fun <- function(x) {round(mean(x))}
			tmpstack <- stack(rlist)
			p1 <- calc(tmpstack, b1fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_1 already exists, skipping calculation, but loading", "\n")
			p1 <- raster(outfile)
		}
		return(p1)
}
