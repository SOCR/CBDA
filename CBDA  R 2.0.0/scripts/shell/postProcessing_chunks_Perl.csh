#!/bin/bash

# Select random lines and columns from a specified file.

jobID=${1?No LONI pipeline job id was specified}
sourceFile=${2?No source file was specified}
workspaceDirectory=${3?No workspace directory was specified}
extractRowCount=${4?No row count to extract was specified}
extractColumnCount=${5?No column count to extract was specified}

# Determine the number of rows in the data
#time fileRowCount=$(cat $sourceFile | wc -l)

# The line below can be run once and then save the info in a file that can be retrieved by each job to save time
time fileRowCount=$(cat $sourceFile | wc -l) > lineCount.txt
#fileRowCount=$(< ${workspaceDirectory}lineCount.txt)
echo $fileRowCount

# How much of the source file to select (i.e. what fraction of lines).
fileFraction=$(echo "scale=5;$extractRowCount/$fileRowCount" | bc)
echo $fileFraction
#fileFraction=0.005
fileFraction_validation=$(echo "scale=5;2*$fileFraction" | bc)
echo $fileFraction_validation

# Determine if the number of rows in the data is smaller than extractRowCount
if (( extractRowCount > fileRowCount)); then
	echo "The number of rows requested, $extractRowCount, is larger than the number of rows in file $sourceFile, $fileRowCount"
	exit 1
fi

## Determine the number of colums in the data set from its first line.
#time fileColumnCount=$(head -n1 $sourceFile | sed 's#,#\n#g' | wc -l)
#
#if (( extractColumnCount > fileColumnCount )); then
#	echo "The number of columns requested, $extractColumnCount, is larger than the number of columns in file $sourceFile, $fileColumnCount"
#	exit 1
#fi
#echo $fileColumnCount


## Random rows selection for the chunks
time perl -ne "print if (rand() < $fileFraction)" $sourceFile  > ${workspaceDirectory}X${jobID}_rows_only_final.txt

## Random rows selection for the validation sets
time perl -ne "print if (rand() < $fileFraction_validation)" $sourceFile  > ${workspaceDirectory}X${jobID}_rows_only_validation_final.txt


## Generate a list of $extractColumnCount random column numbers between 2 and $fileColumnCount.
## It excludes 0 and 1 because the last Perl oneliner by default add the first column (cases/rows ID) and
## the output column (which I set to 1 now, but it can be an input from the command line)
#cols_new_temp=$(echo $(shuf -i 2-$fileColumnCount -n $extractColumnCount | sort -n)) 
#echo $cols_new_temp
#cols_new=$(echo $cols_new_temp | sed 's/ /,/g')
#echo $cols_new

#echo $cols_new > ${workspaceDirectory}k${jobID}.txt

cols_new=$(< ${workspaceDirectory}newk${jobID}.txt)
echo $cols_new

## This Perl oneliner retrieves a subset of random rows and pre randomly-selected columns for each chunk, adding the first column (case ID) and the output column (output_col, which can be an input)
output_col=1
time perl -F',' -lane "print join q(,), @F[split "," $output_col,$cols_new]"  ${workspaceDirectory}X${jobID}_rows_only_final.txt  > ${workspaceDirectory}X${jobID}_final.txt

## This Perl oneliner retrieves a subset of random rows and the same pre randomly-selected columns for each validation set, adding the first column (case ID) and the output column (output_col, which can be an input)
output_col=1
time perl -F',' -lane "print join q(,), @F[split "," $output_col,$cols_new]"  ${workspaceDirectory}X${jobID}_rows_only_validation_final.txt  > ${workspaceDirectory}X${jobID}_validation_final.txt
