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

# workspace_directory = "~/"
#workspace_directory = "/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/2018"

filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
print(filename_specs)
load(filename_specs)

# Consolidation of the Validation Workspaces
for (i in min_covs:max_covs){
  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",i,"_",label,"_VALIDATION.RData"))
  load(filename)
}

filename <- file.path(workspace_directory,
                      paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                             "_k",range_k,"_Light_",label,"_VALIDATION.RData"))
save(list = ls(all.names = TRUE), file = filename)

library(CBDA)
CBDA_object <- CBDA_Stopping_Criteria.pipeline(label = label , Kcol_min = Kcol_min , Kcol_max = Kcol_max,
                                   Nrow_min = Nrow_min , Nrow_max = Nrow_max ,
                                   misValperc = misValperc, workspace_directory = workspace_directory,
                                   M = M , max_covs = max_covs , min_covs = min_covs)

# This loop cleans up all the top ranked validation predictive models
for (j in min_covs:max_covs) {
  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",j,"_",label,"_VALIDATION.RData"))
  file.remove(filename)

}
cat("Clean up completed successfully !!")
