#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
label=as.array(args[1])
workspace_directory=as.array(args[2])

print(label)
print(workspace_directory)

filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
print(filename_specs)
load(filename_specs)
#workspace_directory <- "/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/2018/Simeone/Binomial_10k_1k/"

## Here I check if any job failed, before launchng the consolidation step
#filename1 <- paste0("CBDA_M",M,"_miss",misValperc,"_")
#filename1 <- file.path(workspace_directory,paste0("CBDA_M",M,"_miss",misValperc,"_"))
filename1 <- paste0("CBDA_M",M,"_miss",misValperc,"_")
filename2 <- paste0("_",label,".RData")
print(filename1)
print(filename2)
pattern_temp = paste0("*",filename2) 
#w0=list.files(path = workspace_directory , pattern = "*Binomial_10k_1k_300_NEW.RData")
w0=list.files(path = workspace_directory , pattern = pattern_temp)
print(w0)

jobID_temp <- NULL
for (i in 1:length(w0))
{
  # print(w0[i])
  test1=sub(x=w0[i],filename1,"")
  # print(test1)
  # print(filename2)
  test2=sub(x=test1,filename2,"")
  jobID_temp=c(jobID_temp,test2)
}

#M=30
failedJobs <- NULL
print(jobID_temp)
print(M)
failedJobs <- which(seq(1,as.numeric(M),1) %in% as.numeric(jobID_temp)==FALSE)
#print(failedJobs)
filename_failedJobs <- file.path(workspace_directory,'failedJobs.txt')
#print(filename_failedJobs)
## This checks if it is the 2nd time (at least) that the training has been restarted
## with an existing failedJobs.txt file in the directory. If that is the case,
## the existing file failedJobs.txt is deleted and recreated, if there's any failed job
if (length(list.files(path=workspace_directory,pattern="failedJobs.txt")>0))
{
  cat("Removing the existing file: failedJobs.txt \n\n")
  file.remove(filename_failedJobs) 
}
## This should be empty
#list.files(path=workspace_directory,pattern="failedJobs.txt")
## This saves the failed jobs file in the home directory for easier access
## when thousands of files are being generated during a production run
filename_failedJobs <- 'failedJobs.txt'
if (length(failedJobs)>0)
{ 
  cat("Some jobs failed !! Recreate the  file: failedJobs.txt \n\n")
  write.table(as.matrix(failedJobs),file=filename_failedJobs,row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,
              fileEncoding = "UTF-8", quote = FALSE)
  # If I want to save it as a row
  #write.table(as.matrix(t(failedJobs)),file="failedJobs.txt",row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,
  #            fileEncoding = "UTF-8", quote = FALSE)
  stop(paste0("IMPORTANT: ", length(failedJobs), " jobs failed !!\n See failedJobs.txt for details."))
}
