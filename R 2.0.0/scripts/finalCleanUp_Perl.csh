#!/bin/bash 
# THIS SECTION IS FOR AFTER THE VALIDATION AND STOPPING CRITERIA IS COMPLETED SUCCESSFULLY
# This points to the working directory 
temp_dir=${1?No LONI pipeline working directory was specified}

# This points to the Results subdirectory
part4="Results"

# Here I move all the RData left in the directory after the 
# pipeline workflow is completed successfully AND I delete the newk.txt and *final*.txt files
# The cleanup embedded into the R script works for the RData, not for the text files
files4="*.RData"
files5="newk*"
files6="*final*"
mv $temp_dir$files4 $temp_dir$part4
rm $temp_dir$files5
rm $temp_dir$files6

# Here I list the remaining files in the working directory (it should be 0)
echo "Files remaining in the working directory\n"
echo $(ls $temp_dir* | wc -l)
