#!/bin/bash 
label=$argv[1]
workspace_directory=$argv[2]

label=${1?No label was specified}
workspace_directory=${2?No workspace directory was specified}

/usr/local/R-3.3.3/bin/R --no-save --args $label $workspace_directory < /ifshome/smarino/scripts/CBDA_pipeline_STOPPING_CRITERIA_LG_Perl.R

#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] <  /ifshome/smarino/scripts/CBDA_pipeline_STOPPING_CRITERIA_LG_Perl.R
#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] <  /ifshome/smarino/scripts/CBDA_pipeline_CONSOLIDATION_LG_Perl.R
# > $argv[2]/CBDA_pipeline_CONSOLIDATION_LG_Perl.log

#echo $?
#if  ( $? > 0 ); then
#    echo "The training job failed" ; exit 1
#endif


#if ($?==0) then
#   exit 0
#else
#   exit 1
#endif
