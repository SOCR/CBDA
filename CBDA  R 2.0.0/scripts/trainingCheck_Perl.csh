#!/bin/csh 
# THIS CHECKS HOW MANY RDATA WORKSPACES HAVE BEEN GENERATED DURING THE TRAINING STEP
# IF ANY IS MISSING, A FAILEDJOBS TEXT FILE IS CREATED

temp_dir=$argv[1]
label=$argv[2]

#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] <  /ifshome/smarino/scripts/CBDA_pipeline_TRAINING-CHECK_LG_Perl.R

/usr/local/R-3.3.3/bin/R --no-save --args $temp_dir $label <  /ifshome/smarino/scripts/CBDA_pipeline_TRAINING-CHECK_LG_Perl.R