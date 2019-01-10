#!/bin/csh
# be sure the shell script file "test_csh.csh" is given "execute" file permission with the chmod command:
#  chmod u+x test_csh.csh
echo 'Welcome to the world of script files. Starting ...'
date
#foreach name ( $argv )
 
#   echo Saying hello to $name

#export R_LIBS="usr/local/R-3.3.3/lib64/R/library"
#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] $argv[3] $argv[4] $argv[5] < CBDA_package_test_pipeline_TRAINING_TEST.R > CBDA_package_test_pipeline_TRAINING_TEST.log

#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] $argv[3] $argv[4] $argv[5] < CBDA_package_test_pipeline_TRAINING_TEST_LARGE_SCALE.R > CBDA_package_test_pipeline_TRAINING_TEST_LARGE_SCALE.log

#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] $argv[3] $argv[4] < CBDA_package_test_pipeline_TRAINING_TEST_LARGE_SCALE.R  > CBDA_package_test_pipeline_TRAINING_TEST_LARGE_SCALE_$argv[1].log
#/usr/local/R-3.3.3/bin/R --no-save < test_R344_xgboost.R > test_R344_xgboost_NEW.log
#
#

#/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] $argv[3] < /ifshome/smarino/scripts/CBDA_pipeline_TRAINING_LG_Perl.R > $argv[3]CBDA_pipeline_TRAINING_LG_Perl_$argv[1].log
/usr/local/R-3.3.3/bin/R --no-save --args $argv[1] $argv[2] $argv[3] $argv[4] $argv[5] $argv[6] < /ifshome/smarino/scripts/CBDA_pipeline_TRAINING_LG_Perl_NEW.R
# > $argv[3]CBDA_pipeline_TRAINING_LG_Perl_NEW_$argv[1].log

#echo $?
#if  [ $? > 0 ]; then
#    echo "The training job failed"
#  exit 11
