#!/bin/csh

#jobID={1?No job id was specified}
#label=${2?No label was specified}
#workspace_directory=${3?No workspace directory was specified}

#/usr/local/R-3.3.3/bin/R --no-save --args $jobID $label $workspace_directory < /ifshome/smarino/scripts/CBDA_pipeline_VALIDATION_LG_Perl.R

/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] $argv[3] < /ifshome/smarino/scripts/CBDA_pipeline_VALIDATION_LG_Perl.R
# > $argv[3]CBDA_pipeline_VALIDATION_LG_Perl_NEW_$argv[1].log

#echo $?
#if  [ $? > 0 ]; then
#    echo "The training job failed"
#  exit 11
