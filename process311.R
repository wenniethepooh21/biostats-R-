#MSC1090: Assignment 4 (Part 1 & 2)
#Wennie WU (1000687786)
#OCT182018

#process311.R is an R script that takes a Toronto 311 Services Request Data file
# and determines the total number of service calls per city division, the total number of service calls
# about dead animals on expressways and the ward with the most service calls in September of that year. 


#Calculates the number of service calls per city division, prints the number of calls per each division (string)
NumserviceCalls <- function(dataFile) {
	# Get the number of unique division
	uniqueDivisions <- unique(dataFile[["Division"]])

	#Loop through each unique division and count the number of its occurence in the file
	for(i in uniqueDivisions) {
		# Count the number of times in the Divisions column in dataFile that equals the unique division
		calls <- sum(dataFile$Division == i)
		# Display the output
		cat('\t', i,": ", calls, '\n')
	}
}

#Calculates the number of dead animals reported on the expressway of that year, returns the value of dead animals reported (numeric)
NumDeadAnimals <- function(dataFile) {
	#Counts the number of occurrences "Dead Animal On Expressway" appears in the Service Request Type column
	deadCount <- sum(dataFile$`Service.Request.Type` == "Dead Animal On Expressway")
	# Returns the total number of dead animals on expressway reported 
	return(deadCount)
}

#Determines which ward had the most service calls in September, return the Ward name with the highest calls (string)
MostServiceCalls <-function(dataFile) {
	#Sort data filtering out everything except for the calls made in September in 'Creation Date' column of dataFile
	septCalls <- dataFile[grep("20[0-9]{2}-09-[0-9]{2}", dataFile$`Creation.Date`), ]
	
	#Filter out only the wards column with calls from September
	septWards <- subset(septCalls, select="Ward")
	
	#Calculate the ward with the highest number of calls
		# (Finds the most frequent Ward in the Ward column)
	highestWard<-names(sort(table(septWards), decreasing=TRUE)[1])
	
	#Return the Ward with the highest number of service calls
	return(highestWard)
	
}
######## Main portion of script that calls the above functions ############

# Takes commandline argument from terminal
args <- commandArgs(trailingOnly = TRUE) 

#Error handling to check if user input one commandline argument (data file name)
if(length(args) != 1) {
	# If user didn't input one command line argument, display error message
	cat("Error, please identify one data file to use \n")
} else {
	if (file.exists(args[1])) {
		#Otherwise, use defined one commandline argument, read as a .csv file
		dataFile <- read.csv(args[1])
		#Display the name of the file that was given and that the program is analyzing
		cat("Processing data from file: ", args[1], '\n')
		cat("Total number of service calls per division: \n")
		#Call function to determine the total number of service calls per division
		NumserviceCalls(dataFile)
		#Call function to determine the total number of reported dead animals on the expressway
		numDead <- NumDeadAnimals(dataFile)
		#Display the number returned from NumDeadAnimals function
		cat("The number of reports of a dead animal on an expressway is", numDead, '\n')
		#Call function to determine the ward with the most service calls in September
		ward <- MostServiceCalls(dataFile)
		#Display to user which ward had the most service calls in September of that year
		cat("The ward with the most 311 calls in September was", ward,'\n')
	} else {
		cat("File given as argument does not exist in this directory.\n ")
	}	
}