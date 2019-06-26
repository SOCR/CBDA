#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
dataset_file = as.array(args[1])
workspace_directory=as.array(args[2])
label=as.array(args[3])
maxRowsValidationSet=as.numeric(args[4])
maxColumnsChunks=as.numeric(args[5])
jobID=as.numeric(args[6])
maxRowsChunks=as.numeric(args[7])


print(dataset_file)
print(workspace_directory)
print(label)
print(maxRowsValidationSet)
print(maxColumnsChunks)
print(jobID)
#install.packages("data.table")
library("data.table")

start_time <- Sys.time()

# DETERMINE THE NUMBER OF ROWS OF THE BIG DATA FILE
start_time <- Sys.time()
file1RowCOunt <- nrow(fread(dataset_file, select = 1L))
print(file1RowCOunt)

# DETERMINE THE NUMBER OF COLUMNS OF THE BIG DATA FILE
file1COlumnCOunt <- length(fread(dataset_file,skip=0,nrow=1,sep = ",", header = FALSE))
print(file1COlumnCOunt)

## Determine the rows to randomly select and set aside for the validation step
#maxRowsValidationSet <- 1000
rowsValidationSetCOunt <- round(min(.1*file1RowCOunt,maxRowsValidationSet))
rowsListValidationSet <- sort(sample.int(file1RowCOunt,maxRowsValidationSet))
print(rowsListValidationSet)

filename1 <- file.path(workspace_directory,
                      paste0("rowsListValidationSet.RData"))
save(file1RowCOunt,rowsListValidationSet,file1COlumnCOunt, file=filename1)

#system('time perl -ne "print if (rand() < .005)" Binomial_100k_10k.txt  > X_validation.txt', intern=TRUE)
# rows in a file
#num_rows=$(cat sourceFile | wc -l)


totalCasesList <- 1:file1RowCOunt
trainingCasesList <- totalCasesList[-rowsListValidationSet]
rowsSampleTrainingList <- sort(sample(trainingCasesList,min(length(trainingCasesList),maxRowsChunks)))
ColumnsSampleList <- unique(c(1,sort(sample.int(file1COlumnCOunt,min(file1COlumnCOunt,maxColumnsChunks)))))
print(trainingCasesList)
print(ColumnsSampleList)

## Select the training Set
X_training <- NULL
for (i in 1:length(rowsSampleTrainingList)){
  # this selects a subset of rows and columns
  # I subtract 1 because fread starts indexing from 0
  X_training <- rbind(X_training,fread(dataset_file,skip=rowsSampleTrainingList[i]-1,nrow=1,
                                       select = ColumnsSampleList , sep = ",", header = FALSE))
}

filename <- file.path(workspace_directory,
                      paste0("X",jobID,".RData"))
save(list = ls(all.names = TRUE), file=filename)

end_time <- Sys.time()
end_time - start_time


## Select the Validation Set
# X_validation <- NULL
# for (i in 1:length(rowsListValidationSet)){
#   # this selects a subset of rows and columns
#   # I subtract 1 because fread starts indexing from 0
#   X_validation <- rbind(X_validation,fread(dataset_file,skip=rowsListValidationSet[i]-1,nrow=1 , sep = ",", header = FALSE))
#   print(i/length(rowsListValidationSet))
# }
# 
# system(paste("perl","SFDRv166.pl",arg1,arg2,arg3))
# system(paste("perl -ne","'print if (rand() < .005)'","Binomial_100k_10k.txt",">","test1.txt"))
# 
# perlQuote <- function(string) {
#   escaped_string <- gsub("\\\\", "\\\\\\\\", string)
#   escaped_string <- gsub("/", "\\/", escaped_string)
#   paste("q/", escaped_string, "/", sep="")
# }
# arg1=perlQuote("perl -ne")
# arg2=perlQuote("'print if (rand() < .005)'")
# arg3=perlQuote("Binomial_100k_10k.txt")
# arg4=perlQuote(" > test1_new.txt")
# cmdPerl=paste(arg1, arg2, arg3, arg4)
# arg1=shQuote("perl -ne")
# arg2=shQuote("'print if (rand() < .005)'")
# arg3=shQuote("Binomial_100k_10k.txt")
# arg4=shQuote(" > test1_new.txt")
# cmdShell=paste(arg1, arg2, arg3, arg4)
# system(cmdShell)
# system(paste("perl -ne","print if (rand() < .005)","Binomial_100k_10k.txt > test1_new.txt"),intern = TRUE)
# system(paste("perl -ne","'print if (rand() < .005)'","Binomial_100k_10k.txt > test1.txt"),intern = TRUE)


#time perl -ne 'print if (rand() < .005)' Binomial_100k_10k.txt  > test1.txt


## If the file is 100k rows and 10k columns, the fread is not efficient with preselected random rows
## time perl -ne 'print if (rand() < .005)' Binomial_100k_10k.txt  > test1.txt 
## system('perl -e "print 2 + 4???', intern=TRUE), if perl is installed
## the command perl -ne 'print if (rand() < .005)' Binomial_100k_10k.txt  > test1.txt is very fast
## and extract ~500 rows randomly from the Big Data. If in the original dataset the first column is the row
## number, then the skip for the chunk script can be retrieved form the first column

# filename_RData <- file.path(workspace_directory,
#                       paste0("X_validation_",label,".RData"))
# save(list = ls(all.names = TRUE), file=filename_RData)

# ## Training chunks n=500,p=50
# start_time <- Sys.time()
# ## Set the number of columns to randomly select
# maxRows = 200
# maxCols = 20
# totalCasesList <- 1:file1RowCOunt
# trainingCasesList <- totalCasesList[-rowsListValidationSet]
# rowsSampleTrainingList <- sort(sample(trainingCasesList,min(length(trainingCasesList),maxRows)))
# # This ensure that the first column (output) is always sample for training
# ColumnsSampleList <- unique(c(1,sort(sample.int(file1COlumnCOunt,min(file1COlumnCOunt,maxCols)))))
# print(ColumnsSampleList)
# 
# ## Select the training Set
# X_training <- NULL
# for (i in 1:length(rowsSampleTrainingList)){
#   # this selects a subset of rows and columns
#   # I subtract 1 because fread starts indexing from 0
#   X_training <- rbind(X_training,fread(file1,skip=rowsSampleTrainingList[i]-1,nrow=1, 
#                                        select = ColumnsSampleList , sep = ",", header = FALSE))
# }
# end_time <- Sys.time()
# end_time - start_time

