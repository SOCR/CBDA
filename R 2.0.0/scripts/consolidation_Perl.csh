#!/bin/bash

label=${1?No label was specified}
workspace_directory=${2?No workspace directory was specified}

/usr/local/R-3.3.3/bin/R --no-save --args $label $workspace_directory < /ifshome/smarino/scripts/CBDA_pipeline_CONSOLIDATION_LG_Perl.R
# > $argv[3]CBDA_pipeline_VALIDATION_LG_Perl_NEW_$argv[1].log

#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] < /ifshome/smarino/scripts/CBDA_pipeline_CONSOLIDATION_LG_Perl.R


#echo $?
#if  [ $? > 0 ]; then
#    echo "The training job failed"
#  exit 11
