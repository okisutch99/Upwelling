################################################################################
################################################################################
# The following analysis calculates the spring transition index (STI)
# and compares estimates to those in Bograd et al (2008)
# Modified from S Allen by B. Spence
################################################################################


 rm(list = ls(all = TRUE))
# ==============================================================================
# Libraries/ Settings

  options(warn=2) # =1 for R to continue loop if warning is caught, = 2 For R to abort loop if warning is caught

	 
# ==============================================================================
# Directories/Files.

#  Top.dir <-"."  
 
  	Top.dir <-"C:/Users/brian.spence/My Documents/R/Upwelling" 
    setwd( Top.dir ) 
	
  # =============================================================================
	# STI2014.r
	
	# read file of daily upwelling indices from csv files obtained at   
	# http://www.pfel.noaa.gov/products/PFEL/modeled/indices/upwelling/NA/data_download.html
	
	UPW = read.csv("C:/Users/brian.spence/My Documents/R/Upwelling/36N_Upwelling_2014.txt", 
      header=TRUE, sep = ",", as.is=TRUE)#
  out.fn <- "STI_36N.csv"

  # cbind attaches new columuns to UPW that contain missing values for 6 new variables.
  # next three steps extract yr, mo, and doy out of the YYYYMMDD date format.

	UPW = cbind(UPW, yr=NA, mo=NA, doy=NA, CUI.Jan=NA, CUI.8day=NA, CUI.Fall=NA)
	UPW$yr= as.numeric( substr(UPW$YYYYMMDD, 1,4))	
	UPW$mo= as.numeric( substr(UPW$YYYYMMDD, 5,6))	
	UPW$doy=	strptime(UPW$YYYYMMDD, "%Y%m%d")$yday+1

	###################################!!!!!!!! how to handle NA vals? like for 1997
  # counts no. of missing values, which are coded as "-9999"
  sum(UPW$Index==-9999)
  # assigns missing value NA to all values coded as "-9999"
	UPW$Index[ UPW$Index==-9999] <- NA

  # Interpolate by taking mean of the previous and post upwelling index

  # Find indices of na and non-na values
  # These two steps find the rows for which the value is "NA" (first line) or nor (second line)
	ind.unk <- which( is.na(UPW$Index), arr.ind=TRUE)
	ind.k <- which( !is.na(UPW$Index), arr.ind=TRUE)	
	
  # For each index of an unknown value find the maximum/minimum non NA INDEX value then take the mean.
  # Does the loop from i to length of (ind.unk), which in the current dataset is 20 missing values.
  # Sets L.ind as the index for the last non-missing value prior to the missing value.
  # Sets U.ind as the index for the first non-missing value after the missing value.
  # Assigns the UPW$Index value for the missing value as the mean of the value for L.ind and U.ind
   
	 for (i in 1:length(ind.unk)){
	        L.ind <- max( ind.k[ ind.k < ind.unk[i]])
					U.ind <- min( ind.k[ ind.k > ind.unk[i]])
					UPW$Index[ ind.unk[i]] <- mean( c(UPW$Index[ L.ind], UPW$Index[ U.ind]) )
	 			}
	#Note that this routine would not work if the first or last valus was missing.
	
  # OR Set as zero which is effectively skipping the value in the cumulative sum calculation
  #		UPW$Index[ is.na(UPW$Index)] = 0 
  # Note: this is part of Shanae's original code, which simply assigned zero to any missing values.
  # The results were the same, so she actually used this to test transition dates.  
  # If you ran this after the previous loop, there would be no missing values.  
	
  # Calculate cumulative upwelling from Jan 1 to each day of year.
  # Does loop by year

	for ( i in min(UPW$yr):max(UPW$yr)){
	
			ind.yr <- UPW$yr==i
	    UPW$CUI.Jan[ind.yr] <- cumsum(UPW$Index[ind.yr])
			
	}		


  # Calculate the STI (spring transition date) as the Julian day after the global minimum CUI 
	STI <- NULL
  for ( i in min(UPW$yr):max(UPW$yr)){
	
			ind.min <- which.min(UPW$CUI.Jan[UPW$yr==i] )
			julday.yr <- UPW$doy[UPW$yr==i]
			# Set STI as day after minimum as long as it's not the last day of the year
			if (julday.yr[max(ind.min)]==max(julday.yr)){
				  sti = julday.yr[max(ind.min)]
			}else{
			    sti = julday.yr[max(ind.min) +1]
			}
	    STI <- c( STI, sti ) 	
    	}		
  #  View(UPW[UPW$yr == 1967,])	# This prints just the 1967 values to allow for a check that the
  #  calclations were done correctly.

	names(STI) <- as.character( seq(min(UPW$yr),max(UPW$yr)))
  
	
	# Compare to Bograd et al (2009) - using data from 1967-2007 - 39N: mean=50, sd=34, 
																																#42N: 82 +/- 29
																																#48N: 119 ? 29  									
	mean(STI[ names(STI)  %in% seq(1967, 2007)])
	sd(STI[ names(STI)  %in% seq(1967, 2007)])
			
	# Use 153 as 1993 value	(this was a mistake in Bograd et al (2009))	
	yrs <- c( seq(1967,1992), seq(1994, 2007))
	mean( c( STI[ names(STI)  %in% yrs], 153))
	sd(c( STI[ names(STI)  %in% yrs], 153))
		

  # write to file 

  write.csv( STI, file=out.fn)
	