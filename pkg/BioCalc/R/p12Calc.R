# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p12Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Total annual rainfall (P12)", "\n")
			tmpstack <- stack(rlist)
			b12fun <- function(x) {sum(x)}
			p12 <- calc(tmpstack, b12fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_12 already exists, skipping calculation, but loading", "\n")
			p12 <- raster(outfile)
		}
		return(p12)
}
