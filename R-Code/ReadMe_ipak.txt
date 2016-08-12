This is the complete list of packages used so far to run the CBDA-SL algorithm:

LIST OF PACKAGES
"ggplot2", "plyr","dplyr", "colorspace","grid","data.table","VIM","MASS","Matrix", "lme4","arm","foreach","glmnet","class","nnet","mice","missForest", "calibrate", "nnls","SuperLearner","plotrix","TeachingDemos","plotmo","earth","parallel","splines","gam", "Amelia", "Hmisc", "mi","BayesTree","e1071","randomForest","bartMachine"

the command to install each one is the following:

install.packages(package_name, dependencies = TRUE, repos='http://cran.rstudio.com/')


There is a function (ipak.R) in the repository that does all of that automatically. Whoever is doing the installation, 
can use that function to expedite everything.
In order to use it, this is what needs to be executed:

packages <- c("ggplot2", "plyr","dplyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam", "Amelia", "Hmisc", "mi",
              "BayesTree","e1071","randomForest","bartMachine")
source('ipak.R') # or wherever the file is located
ipak(packages)
