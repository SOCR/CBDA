# be sure the shell script file "test_csh.csh" is given "execute" file permission with the chmod command:
#  chmod u+x test_csh.csh
echo 'Welcome to the world of script files. Starting ...'
date
#
 export LIBRARY_PATH="/usr/local/pgi-7.2-5/linux86/7.2-5/lib/"
#
/ifshome/shobel/R-3.3.1/bin/R --no-save < test_R344_xgboost.R > test_R344_xgboost_NEW.log
#/usr/local/R-3.4.4/lib64/R/bin/exec/R < test_R344_xgboost.R > test_R344_xgboost_NEW.log
#
echo 'Ending ...'
date
#
exit 1
