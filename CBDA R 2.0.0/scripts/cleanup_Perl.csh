#!/bin/csh 
# be sure the shell script file "test_csh.csh" is given "execute" file permission with the chmod command:
#  chmod u+x test_csh.csh
echo 'Welcome to the world of script files. Starting ...'
date
#foreach name ( $argv )
 
#   echo Saying hello to $name

#export R_LIBS="usr/local/R-3.3.3/lib64/R/library"
#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] $argv[3] $argv[4] $argv[5] < TEST_for_Yi_Lu.R > TEST_for_Yi_Lu.log


#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] < CBDA_package_test_pipeline_CONSOLIDATION_TEST_LARGE_SCALE.R > CBDA_package_test_pipeline_CONSOLIDATION_TEST_LARGE_SCALE.log


/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] <  /ifshome/smarino/scripts/CBDA_pipeline_CLEANUP_LG_Perl.R
# > $argv[2]/CBDA_pipeline_CONSOLIDATION_LG_Perl.log

#echo $?
#if  [ $? > 0 ]; then
#    echo "The training job failed"
#  exit 11
