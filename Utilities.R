#MSC1090: Assignment 7
#Wennie WU (1000687786)
#NOV082018

#Utilities.R is an Rscript that contains functions to perform statistical analysis on a given data

#This function attempts to read in data from a given csv file
#Returns the read in data frame
#Assumes the file is in the current directory
library(scatterplot3d)
readData <- function(filename) {
	fileLocation <- paste("./", filename, sep = "")
	tryCatch ({
		dataFrame <- read.csv(fileLocation, header = TRUE)
		}, error = function(e) {
		#Outputs an error if the file doesn't exist 
		stop("CSV file doesn't exist in current directory, please try again ")
	
	})
	return(dataFrame)

}

combData <- function(fileOne, fileTwo) {
	combinedData <- cbind(fileOne, fileTwo)
	return (combinedData)
}
#cLtester is a function that takes a string as an argument, given in the command line to check if an appropriate argument was given by the user
#returns TRUE if the user provided an acceptable argument, provides an error message if the user fails to provide an acceptable argument
cLTester <- function(argument) {
	if (length(argument) != 1) {
			stop("You did not input a valid argument, please provide either:
			1 - for a linear implementation of the data 
			2 - for a quadratic model of the data or 
			3 - for both a linear and quadratic implementation of the data
			11 - for a linear implementation of the data and analyze the given model
			12 - for a quadratic implementation of the data and analyze the given model
			21 - for a linear implementation of the data and analyze the given model, remove suspicious points then re-run analysis
			22 - for a quadratic implementation of the data and analyze the given model, remove suspicious points then re-run analysis
			30 - for a generalized linear model fit of the data ")
	} else {
		#The program will stop if the user gives a single argument that isn't either 1, 2 or 3
		if (argument[[1]] == "1" |argument[[1]] == "2"|argument[[1]] == "3"|argument[[1]] == "11"
			| argument[[1]] == "12"|argument[[1]] == "21"|argument[[1]] == "22"|argument[[1]] == "30"){
			return(TRUE)
		} else {
			stop("You did not input a valid argument, please provide either:
			1 - for a linear implementation of the data 
			2 - for a quadratic model of the data or 
			3 - for both a linear and quadratic implementation of the data
			11 - for a linear implementation of the data and analyze the given model
			12 - for a quadratic implementation of the data and analyze the given model
			21 - for a linear implementation of the data and analyze the given model, remove suspicious points then re-run analysis
			22 - for a quadratic implementation of the data and analyze the given model, remove suspicious points then re-run analysis
			30 - for a generalized linear model fit of the data ")
		}
		
	} 
}


#This function computes the correlation of the dataset given
#Displays the covariance, correlation and Pearson's Test values to the user to understand the relationship between the data points
computeCorrelation <- function(datax, datay) {
	cat("Computing correlation indicators...\n")
	covariance <- cov(datax, datay)
	cat("Covariance: ", covariance, '\n')
	correlation <- cor(datax, datay)
	cat("Correlation coefficient: ", correlation, '\n')
	cat("Correlation Test: \n")
	print(cor.test(datax, datay))
	
}

#This function creates a linear model of the given dataset, it returns to the user the correlation data by calling computeCorrelation and plots a linear graph of the given data
#Returns the data computed by the linear model
linearModel <- function(data) {
	data <- data[order(data[[1]]),]
	computeCorrelation(data[[1]],data[[2]])
	cat("-----------------------------\n")
	cat("Fitting a Linear Model \n")
	linModel <- lm(data[[2]] ~ data[[1]]) 
	print(summary(linModel))
	cat("-----------------------------\n")
	plotLinear(data[[1]],data[[2]], linModel)

	return(linModel)
}
 
#This function creates a quadratic model of the given dataset, it returns to the user the correlation data by calling computeCorrelation and plots a quadratic graph of the given data
#Returns the data computed by the quadratic model
quadraticModel <- function(data) {
	#Order the data by x values in ascending order (for plotting purposes)
	data <- data[order(data[[1]]),]
	computeCorrelation(data[[1]], data[[2]])
	cat("-----------------------------\n")
	cat("Fitting a quadratic Model \n")
	datax2 <- data[[1]]^2
	QuadModel <- lm(data[[2]] ~data[[1]] + datax2) #Use the linear model function in R to calculate the quadratic model 
	print(summary(QuadModel))
	cat("-----------------------------\n")
	plotQuadratic(QuadModel,data[[1]],data[[2]],datax2)
	return(QuadModel)
}

#This function creates a linear and model of the given dataset, it returns to the user the relationship between the data points in a linear model and in a quadratic model and plots them on a single graph
linQuadModel <- function(data) {
	linearM <- linearModel(data) #Get calcualtions from linear model 
	quadraticM <- quadraticModel(data) #Get calculations from quadratic model
	plotLinearQuad(linearM,quadraticM, data[[1]], data[[2]]) #Plot the data given by linearM and quadraticM (models)

}

#This function plots the linear model given the linear model data and the given x and y data points 
plotLinear <- function (datax,datay, model) {
	plot(datax, datay)
	abline(model)
}

#This function plots the quadratic model given the quadModel data computed in quadraticModel and the given x and y data points
plotQuadratic <- function(quadModel,datax,datay,datax2){
	quadModel2 <- quadModel$coef %*% rbind(1, datax, datax2) #follow Ax+b formula to calculate the quadratic equation 
	plot(datax,datay)
	#Plot the quadratic curve
	lines(datax, quadModel2, lwd = 2, col = 2)
}

#Plots both the linear and quadratic models in one plot using the quadModel data computed in quadraticModel, the linModel data computed in linearModel and the given x and y data points
plotLinearQuad <- function(linModel, quadModel, datax,datay) {
	plot(datax,datay)
	abline(linModel)
	lines(datax,quadModel, lwd=2, col =2)
}

#This function should generate several diagnostic plots based on the given model with suspicious points highlighted according to the given tolerance level or default level of 0.25
#Function displays to the terminal the suspicious points 
#Function returns a list of the suspicious points according to the tolerance level
AnalyzeModel <- function(model, dataPoints, tolerance = 0.25) {
	if(tolerance > 0 && tolerance < 1) {
		#leverage of an observation measures the ability to move the regression model
		lev <- hat(model.matrix(model))
		plot(lev)
		#Cook's distance
		cookDistance <- cooks.distance(model)
		csusPointsIndex <- which(cookDistance > tolerance) #suspicious points are when cook distances are > tolerance
		plot(cookDistance, ylab = "Cook's Distance")
		points(csusPointsIndex, cookDistance[csusPointsIndex], col = 'red') #colour the suspicious points red
		if(length(csusPointsIndex > 0)) {
			suspiciousPoints <- dataPoints[csusPointsIndex,]
			cat("Suspicious points in the dataset are: \n")
			print(suspiciousPoints)
		} else {
			cat("No suspicious points detected in the dataset. \n")
			suspiciousPoints <- csusPointsIndex
		}
		#residual plots 

		#residuals show a statistical estimation of the difference between the model and the actual data
		resid <- model$res
		#Plot the residual data
		par(mfrow = c(1,3))
		plot(dataPoints[[1]], resid)
		#Highlight the suspicious points in each residual plot
		points(dataPoints[csusPointsIndex,1],resid[csusPointsIndex], col = 'red')
		plot(dataPoints[[2]], resid)
		points(dataPoints[csusPointsIndex,2],resid[csusPointsIndex], col = 'red')
		#Fitted model
		plot(model$fitted, resid)
		points(model$fitted[csusPointsIndex],resid[csusPointsIndex], col = 'red')

		#Studentized(Standardized) residuals will compensate for different leverage
		studentizedRes <- rstudent(model)
		#Plot the standardized data
		par(mfrow = c(1,3))
		plot(dataPoints[[1]], studentizedRes)
		#Highlight the suspicious points in each standardized residual plot
		points(dataPoints[csusPointsIndex,1],studentizedRes[csusPointsIndex], col = 'red')
		plot(dataPoints[[2]], studentizedRes)
		points(dataPoints[csusPointsIndex,2],studentizedRes[csusPointsIndex], col = 'red')
		#Fitted model 
		plot(model$fitted, studentizedRes)
		points(model$fitted[csusPointsIndex],studentizedRes[csusPointsIndex], col = 'red')
		
		#Show the residual distribution - normal probability plot of the calculated residuals to check the normality assumption
		#Plot a histogram of the residuals
		hist(resid)

		#qq-plot of the residual distribution (normalized) with a line
		qq <- qqnorm(resid)
		#highlight the suspicious points in qq normalized plot
		points(qq$x[csusPointsIndex],qq$y[csusPointsIndex], col = 'red')
		qqline(resid)
		
		return(suspiciousPoints)
	} else {
		stop("Invalid tolerance value given. Tolerance level should be between 0 and 1 non-inclusive")
	}
}

#This function removes the suspicious data points determined when the Cook's distance is larger than the tolerance
#Returns dataset without the suspicious data points
cleanSuspicious <- function(susPoints, data) {
	#Find the indexes to remove from the dataset given the suspicious points
	#Check if there were detected susPoints
	if(length(susPoints) > 0) {
		cleanDataInd<- which(data[[1]] %in% susPoints[[1]] & data[[2]] %in% susPoints[[2]])
		#select all the rows except those that matched the susPoints
		cleanedData <- data[-cleanDataInd,]
		#Return the "cleaned" dataset
		return(cleanedData)
	} else {
		#no suspicious points were detected, return original data points back.
		return(data)
	}

}

#the function should return the model created and generate a 3D 
#plot with the data points and the fitted "plane" for them.
premModel <- function(data) {
	#Use a generalized linear model to fit the data 
	gfit <- glm(data[[3]] ~ data[[1]] + data[[2]], family = binomial)
	#Generate a 3D plot for the data points (premature as a function of height and head circumference)
	scatter <- scatterplot3d(data[[1]], data[[2]], data[[3]])
	#Fitted plane 
	scatter$plane3d(gfit)
	#Return the model 
	return (gfit)

}









