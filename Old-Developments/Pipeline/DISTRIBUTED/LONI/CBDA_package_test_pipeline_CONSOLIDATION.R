#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)
arg_file = as.array(args[1])
print(arg_file)
j_global=as.numeric(args[2])
alpha=as.numeric(args[3])
dataset_file = as.array(args[4])
workspace_directory=as.array(args[5])
label=as.array(args[6])
i_exp=as.numeric(args[7])

print(label)
print(workspace_directory)
print(j_global)
print(alpha)

# label = "Binomial"
# workspace_directory = "~/"
library(CBDA)
CBDA_Consolidation.pipeline(top , max_covs , M , misValperc ,  range_k , range_n ,
                            label = label, workspace_directory = workspace_directory)

