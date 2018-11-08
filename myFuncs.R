#MSC1090: Assignment 3 (Part 1)
#Wennie WU (1000687786)
#11OCT2018

# myFuncs.R file contains 2 R functions SpenceMean and PonceMean

# SpenceMean function takes a vector of integers as an argument and calculates the 'Spence' mean from those values
SpenceMean <- function(x) {
	#Check if values in x are of type numberic
	if(is.numeric(x)) {
		#Find the number of values where x > 0
		n <- length(x[x > 0])
		
		#Check the length of n 
		if (n > 0) {
			#Find the values where x > 0 to be used for the Spence mean calculations
			xValsGT0 <- x[x > 0]
			
			#Insert values into Spence mean equation - the exponential value of the sum of all log x values > 0 raised to the power of 1/n
			SMean = exp(sum(log(xValsGT0 ^ (1/n))))
			
			#return the spence mean value of x
			return(SMean)
		} else {

			#Error occurred, display meaningful error message
			stop(paste("Error in SpenceMean function: no values in x is greater than 0."))

		}
	} else {
		#Error occurred, display meaningful error message
		stop(paste("Error in SpenceMean function: x must be numeric."))
		
		
	}
}


# PonceMean function takes a vector of integers and a value m as arguments, m being optional and calculates the 'Ponce' mean from those values
PonceMean <- function(x, m = 1) {
	# Check if values of x are numeric
	if(is.numeric(x)) {

		#Check if m is numeric and is positive
		if(is.numeric(m) && m > 0) {

			# Find the number of values where x >= 0
			n <- length(x[x >= 0])
			if (n > 0) {
				# Find the values where x > 0 to be used for the Ponce mean calculations
				xValsGT0 <- x[x >= 0]
				
				# Insert values into Ponce mean equation - product of all x values >= 0 raised to the power 1/, then all raised to the power m/n
				PMean = (prod(xValsGT0) ^ (1/m)) ^ (m/n)
				
				# Return the Ponce mean value of x and m (if given)
				return(PMean)
			} else {
				#Error occurred, display meaningful error message
				stop(paste("Error in PonceMean function: no values in x are greater than or equal to 0."))
			}
		} else {
			#Error occurred, display meaningful error message
			stop(paste("Error in PonceMean function: m must be positive."))
		}
	} else {
		# x values are not numeric, error message will be displayed
		# Check if m is greater than 0
		if(m > 0) {
			#Error occurred, display meaningful error message
			stop(paste("Error in PonceMean function: x must be numeric."))
		} else {
			#Error occurred, display meaningful error message
			stop(paste("Error in PonceMean function: x must be numeric and m must be positive."))
		}

	}	
}