#Assignment 1
# Wennie WU (1000687786)
#20SEP2018

#!/bin/bash
#Output the file name given as an argument
echo "Working with data file ${1}."

#Count the number of patients there are with the identifier
numPatients="$(grep 'ILMN_*' ${1} | wc -l)"

#Output the number of patients there are
echo "The total number of patients is ${numPatients//[[:blank:]]/}." 

#Count the number of patients without any null values
compData="$(grep 'ILMN_*' ${1}| grep -v 'null' | wc -l)"

#Output the number of patients without any null values
echo "The number of patients with complete data is ${compData//[[:blank:]]/}."

#### Spence answers ####
#numP = $(grep ILMN ${1} | wc - l)

#numGood = $(grep ILMN ${1} | grep -v null | wc -l)

#echo "working with data faile ${1}."
#echo "total number of pateints is ${numP}."
#echo "number of patients with complete data is ${numGood}."