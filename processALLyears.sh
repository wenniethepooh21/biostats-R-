#MSC1090: Assignment 4 (Part 3)
#Wennie WU (1000687786)
#OCT182018

# This bash script will run through all 311 CSV files in the current directory calling process311.R Rscript
# Will execute .csv files sequentially by year.

#!/bin/bash
for file in *.csv  
do
	Rscript process311.R $file
done
