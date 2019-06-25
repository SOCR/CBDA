#!/usr/bin/env Rscript
#args = commandArgs(trailingOnly=TRUE)
#print(args)

# THIS CHECKS THE VERSION OF R INSTALLED
version

# THIS CHECKS IF XGBOOST IS INSTALLED
version
print ("Attaching CBDA, xgboost,Superlearner, FNN and SMOTE package....")
library(CBDA)
library(smotefamily)
library(SuperLearner)
library(bartMachine)
library(bartMachineJARs)
library(xgboost)
library(FNN)
print ("Status OK ... Exiting!")

#q()
