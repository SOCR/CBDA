## VERY IMPORTANT -- SCRIPT 1 [FOR THE PIPELINE WORKFLOW]
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)
arg_file = as.array(args[1])
#print(arg_file)
#j_global=as.numeric(args[2])
dataset_file = as.array(args[2])
#workspace_directory=as.array(args[4])
workspace_directory=as.array(args[3])
#i_exp=as.numeric(args[5])

# label to append to the RData workspaces as soon as they are created
label=c("ADNI_dataset")
# /ifshome/pipelnvt/ # home directory as aguest on LONI Pipeline on "Cranium"

#Set the list of packages/libraries to install/include (done through the ipak.R function)
packages <- c("ggplot2", "plyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam","mi",
              "BayesTree","e1071","randomForest", "Hmisc","dplyr","Amelia","bartMachine","knockoff")

## ipak function below: install (if missing) and load (if installed) multiple R packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}

#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')
ipak(packages)

# arg_file = as.array('C:/Users/simeonem/Documents/CBDA-SL/Cranium/arg_ABIDE.txt')
# workspace_directory<-c("C:/Users/simeonem/Documents/CBDA-SL/ExperimentsNov2016/ABIDE/")
# dataset_file = c("ABIDE_dataset.csv")

# Reads the dataset to be processed as passed from the input argument
#dataset_file = c("C:/Users/simeonem/Documents/CBDA-SL/Cranium/ABIDE.csv")
#setwd(workspace_directory)
eval(parse(text=paste0("Data = read.csv(dataset_file, header = TRUE)")))
Data = read.csv(dataset_file, header = TRUE)
eval(parse(text=paste0("arguments = read.table(arg_file, header = TRUE)")))
#print('1')

Data$researchGroup <- ifelse(Data$researchGroup == "Autism",1,0)
Data$subjectSex<- ifelse(Data$subjectSex == 'M',1,0)

Ytemp <- Data$researchGroup # col 9
original_names_Data <- names(Data)
#Ytemp_MN <- Data$Dx_Category  # col 11
# handedness is col 99 with 5 meaningful categories, 315 NA and 11 -9999 --> ELIMINATE
cols_to_eliminate=c(1:8,9,11:12,14:39,96:98,99,107:110,303,310,342,346,349,353,389,396)
Xtemp <- Data[-cols_to_eliminate]
original_names_Xtemp <- names(Xtemp)
# SET THE SAME NAMES/LABELS FOR THE X dataset
names(Xtemp) <- 1:dim(Xtemp)[2]

## SAMPLE THE PREDICTION DATASET -- THIS STEP IS DATA INDEPENDENT
## The fraction alpha of data/patients to use for prediction could be passed as an input argument as well
## Below the sampling is balanced
## Eliminating q subjects for prediction, in a BALANCED WAY
alpha = 0.30; # % of the initial subjects to set aside for prediction
## Subjects to sample in a balanced way from 0-1 outcomes
a0 = round(alpha*dim(Xtemp)[1]/2)
# Randomly select patients for prediction
q1 = sample(which(Ytemp==1),a0)
q2 = sample(which(Ytemp==0),a0)
q <- c(q1 , q2)

Xpred <- Xtemp[q,] # define the feature set for prediction (renamed Xpred) [not used in the training/learning]
Ypred <- Ytemp[q] # define the output for prediction (renamed Ypred) [not used in the training/learning]

eval(parse(text=paste0("save(arguments,label,workspace_directory,Xpred,Ypred,q,
                       Xtemp,Ytemp,cols_to_eliminate,original_names_Data,original_names_Xtemp,
                       file= \"~/ABIDE_ValidationSets_Specs.RData\")")))