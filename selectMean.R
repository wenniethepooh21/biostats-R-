#MSC1090: Assignment 2 (Part 2)
#Wennie WU (1000687786)
#04OCT2018

# Creating Rscript that runs either the SpenceMean function or PonceMean function defined in 'myFuncs.R' depending on the given command line argument

# Give default vector values
x <- c(-10,-3, 2, 6, 4, 5, 0.3, -0.1, 2.1, 3.2, 12.1, -12.1, -1.2) 

# Call 'myFuncs.R'
source("myFuncs.R")

# Store command line arguments into 'args'
args <- commandArgs(trailingOnly = TRUE)


# Check given command line arguments, should be provided with one.
if (length(args) == 0 || length(args) > 1) {
	cat("Error, please provide a valid argument.", '\n')
# More than 1 argument was given, return an error message 
}
# Check to see what command line argument was given
 if(length(args) == 1) {	
	if(args[1] == "Spence") { 
		#'Spence' arugment given, run SpenceMean function passing x as an argument into the function
		spenceData <- SpenceMean(x)
		# Display the result from SpenceData function to screen
		cat("The Spence mean of the data is", spenceData, '\n')

	} else if (args[1] == "Ponce") {
		# 'Ponce' argument given, run PonceMean function passing x and 4 as arugments into the function	
		ponceData <- PonceMean(x, 4)
		# Display the result from SpenceData function to screen
		cat("The Ponce mean of the data, with m = 4, is", ponceData, '\n')
	} else {
		# Neither 'Ponce' or 'Spence' was given as an argument
		cat ("Error, incorrect command line argument.", '\n')
	}
}

