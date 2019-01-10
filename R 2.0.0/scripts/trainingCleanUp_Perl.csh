#!/bin/bash 
# THIS SECTION IS FOR AFTER THE TRAINING AND CONSOLIDATION IS COMPLETED SUCCESSFULLY
label=${1?No label was specified}
temp_dir=${2?No LONI pipeline working directory was specified}
M=${3?No M was specified}

echo $label
echo $temp_dir
echo $M

# This points to M text files (feature sets, training only)
files1="k*.txt"
# This removes these M text files
rm $temp_dir$files1

# This points to M*2 text files (rows_only, training and validation sets)
files2="*rows*.txt"
# This removes these 2*M text files
rm $temp_dir$files2

# This points to M*2 text files (validation sets).
# Because of the rm commands above, only M text files are left to clear
files3="*validation.txt"
temp_files=$temp_dir$files3
rm $temp_files
files31="X*.txt"
temp_files=$temp_dir$files31
rm $temp_files

# Here I copy the consolidated training RData into the Results subdirectory
# CBDA_M9000_miss0_Binomial_10k_1k_9000
part1="CBDA_M"
part2="_miss0_"
part3=".RData"
part4="Results"
trainingRData=$part1$M$part2$label$part3
cp $temp_dir$trainingRData $temp_dir$part4
files4="*.RData"
cp $temp_dir/$files4 $temp_dir$part4

