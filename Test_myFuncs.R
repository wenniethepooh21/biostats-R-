#MSC1090: Assignment 3 (Part 2)
#Wennie WU (1000687786)
#11OCT2018

# Test_myFuncs.R is an R script that tests the functions in the myFuncs.R file

# source the file containing the functions to be tested
source("myFuncs.R")

# Testing functions with the following vectors 
# vector with values 1-10
testa <- c(1:10)
# vector with only the value -10
testb <- c(-10:-10) 
# Strange vector test
testc <- c("hello", "this","should","not","work")
# vector values with negatives and positive values
testd <- c(-2, 0, 0.5, 7, 0, 4, 4, -2)

#Testing if the given x returns expected output from SpenceMean(x) or Ponce(x, m = 1) function
calcTest <- function(x, expected, test.name, function.name) {
	if(x == expected) {
		cat(function.name, "passed", test.name, "test. \n")
	} else {
		cat(function.name, "failed", test.name, "test. \n")
	}
}

# Testing error handling messages
errorTest <- function(y, test.name, function.name, m = 1) {
	# initialize testError flag to 'FALSE', if no error is called, test failed
	testError = FALSE

	# test the function:
	# tryCatch will be used to check if error message was displayed to screen, 
		# but won't display the error message on screen
	testError = tryCatch({
		# Run test on given argument
		if(function.name == "spence mean") {
			#Execute SpenceMean function
			SpenceMean(y)
		} else {
			#Execute PonceMean function
			PonceMean(y, m)
		}
	} # if either function has an error message displayed, it will execute the following and the error message is 'caught'
	 , error = function(e) {
	 	# An error should have occurred, set flag to 'TRUE', the test passed
	   testError = TRUE
	})

	#Display to screen if test failed or passed
	if(testError == TRUE) {
		# Error message was displayed, test is correct
		cat(function.name,"passed", test.name, "test. \n")
	} else {
		# Error message wasn't displayed, test failed
		cat(function.name, "failed", test.name, "test. \n")
	}
}

# Test cases for SpenceMean function
calcTest(sprintf("%.6f", SpenceMean(testa)), 4.528729, "1-10", "spence mean")
calcTest(sprintf("%.6f", SpenceMean(testd)), 2.735565, "[-2, 0, 0.5, 7, 0, 4, 4, -2]", "spence mean")
errorTest(testb, "-10-10", "spence mean") #error
errorTest(testc, "strange vector", "spence mean") #error

#Test cases for PonceMean function 
# use a negative m value for ponce mean to test error handling
negM <- -2 
calcTest(sprintf("%.6f", PonceMean(testa)), 4.528729, "[1-10]", "ponce mean")
calcTest(sprintf("%.0f", PonceMean(testd)), 0 , "[-2, 0, 0.5, 7, 0, 4, 4, -2]", "ponce mean")
errorTest(testb, "-10-10", "ponce mean")
errorTest(testc, "strange vector", "ponce mean")
errorTest(testd, "negative m value", "ponce mean", negM)
errorTest(testc, "strange vector and negative m", "ponce mean", negM)
