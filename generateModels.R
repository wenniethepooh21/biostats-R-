#MSC1090: Assignment 6
#Wennie WU (1000687786)
#NOV012018

# Rscript that tests the functionality of all functions in Utilities.R to study the relationship
	#between a child's height (inches) and their head circumference (inches) or any given data 



source('Utilities.R')
#When testing this Rscript, the following dataFilen name will have to be changed. Please load in a .CSV file with the data
dataFile = "../assn6/heightCircData.csv"

#Call the readData file to read in the data file with the datasets you want to analyze
data = readData(dataFile)

newDataFile = "premature.csv"

premData = readData(newDataFile)

heightCircPremData = combData(data, premData)

args = commandArgs(trailingOnly = TRUE)


#Defensive programming, to see if the user input the correct and acceptable command line argument
if(cLTester(args)) {
	if(args[1] == "1") {
		lin<-linearModel(data)
		#The plot generated from the linear model shows the line beind in the middle of all the plotted points, except for a few outliers. 
		# The linear model seems to be a good fit to the dataset provided indicating that there is a linear relationship between height and head size
	} 
	else if (args[1] == "2") {
		quad<-quadraticModel(data) 
		#The plot generated from the quadratic model shows a line very similar to the linear model. This suggests that the linear model is better suited to represent
		# the given data. The relationship between height and head circumference is therefore, not really a quadratic, but more linear
	}
	else if(args[1] == "3") {
		linquad<-linQuadModel(data)
		# When both linear and quadratic plots are shown, we can clearly see that the quadratic model and linear model are pretty much the same. This highly indicates 
		# that the given data have a linear relationship with each other
	}
	else if(args[1] == "11") {
		#Call the linear model and analyze it
		linear <- linearModel(data)
		AnalyzeModel(linear, data)

	}
	else if(args[1] == "12") {
		#Call the quadratic model and anayze it
		quadrat <- quadraticModel(data)
		AnalyzeModel(quadrat, data)

	}
	else if(args[1] == "21") {
		#Call the linear model and anayze it then remove the 'suspicious' points
		linear <- linearModel(data)
		suspicious <- AnalyzeModel(linear, data)
		#Call the linear model with the suspicious points removed from dataset
		newPoints <- cleanSuspicious(suspicious, data)
		newLinear <- linearModel(newPoints)
	}
	else if(args[1] == "22") {
		#Call the linear model and anayze it then remove the 'suspicious' points
		quadratic <-quadraticModel(data)
		suspicious <- AnalyzeModel(quadratic, data)
		#Call the quadratic model with the suspicious points removed from dataset
		newPoint <- cleanSuspicious(suspicious, data)
		newQuad <- quadraticModel(newPoint)

	} 
	else if(args[1] == "30") {
		#implement a model to fit if the babies were premature or not, as a function of the height and 
		#circumference measurements.
		modelFit <- premModel(heightCircPremData)
	}
}
