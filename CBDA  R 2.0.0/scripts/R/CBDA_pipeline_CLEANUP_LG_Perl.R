#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
label=as.array(args[1])
workspace_directory=as.array(args[2])

filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
#filename_specs <- file.path(workspace_directory,paste0("_validation_info.RData"))

print(filename_specs)
load(filename_specs)

## This section removes all unnecessary files,
## after the CBDA experiment completed successfully

cat("Removing unnecessary files (_VALIDATION.RData,newk.txt, rows_only_final.txt 
    and rows_only_validation_final.txt)\n\n")
for (j in min_covs:max_covs) {
  filename <- file.path(workspace_directory,
                        paste0("CBDA_M",M,"_miss",misValperc,
                               "_",j,"_",label,"_VALIDATION.RData"))
  
  file.remove(filename)
}

w1=list.files(path=workspace_directory,pattern="*500_")
print(w1)
file.remove(path=workspace_directory,w1)

for (i in 1:length(w1)){
  w1_temp=paste0(workspace_directory,w1[i])
  unlink(w1_temp,force = TRUE)
}
w2=list.files(path=workspace_directory,pattern="^X")
print(w2)
file.remove(path=workspace_directory,w2)
for (i in 1:length(w2)){
  w2_temp=paste0(workspace_directory,w2[i])
  unlink(w2_temp,force = TRUE)
}
#unlink(w2,force = TRUE)
cat("Unnecessary files removed\n\n")
cat("Clean up completed successfully !!\n\n")
