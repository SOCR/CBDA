# working directory where the files are retrieved from and where they are saved
workspace_directory <- setwd("~/Downloads")
# specs for the experiments to run
arg_file <- c("arg_Binomial_new_Test.txt")
# dataset to analyze
dataset_file <- c("Binomial_dataset_new_sd.txt")
# label to append to the RData workspaces as soon as they are created
label <- c("Binomial_dataset_new_sd")

#Set the list of packages/libraries to install/include (done through the ipak.R function)
packages <- c("tidyr","ggplot2", "plyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam","mi",
              "BayesTree","e1071","randomForest", "Hmisc","dplyr","Amelia","bartMachine",
              "knockoff","caret")

## ipak function below: install (if missing) a nd load (if installed) multiple R packages
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
    sapply(pkg, require, character.only = TRUE)
}

#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')
ipak(packages)

# Reads the dataset to be processed as passed from the input argument
Data = read.csv(dataset_file, header = TRUE)
arguments = read.table(arg_file, header = TRUE)

Ytemp <- Data[,1] 
original_names_Data <- names(Data)

cols_to_eliminate=1 # only the outcome in this example
Xtemp <- Data[-cols_to_eliminate]
original_names_Xtemp <- names(Xtemp)

# SET THE SAME NAMES/LABELS FOR THE X dataset
names(Xtemp) <- 1:dim(Xtemp)[2]

## SAMPLE THE PREDICTION DATASET -- THIS STEP IS DATA INDEPENDENT
## This ensures reproducibility of the analysis
set.seed(12345)
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
print(arguments)

Specs_File = paste0(workspace_directory,"/",label,"_Specs.RData")
save(arguments,label,workspace_directory,Xpred,Ypred,q,Xtemp,Ytemp,cols_to_eliminate,
     original_names_Data,original_names_Xtemp,file= Specs_File)

# Experiment to run. If it is commented out, it will perform a set of experiments specified in the loop below
# that goes from 1 to # of rows in the specs file loaded into arguments
#i_exp <- 1
# CBDA iteration to run. If it is commented out, then it will If it is commented out, then it will perform a set of jobs specified in the loop below
# that goes from 1 to M
j_global <- 1
# Specs_File = paste0(workspace_directory,"/",label,"_Specs.RData")
# # save(arguments,label,workspace_directory,Xpred,Ypred,q,Xtemp,Ytemp,cols_to_eliminate,
# #      original_names_Data,original_names_Xtemp,file= Specs_File)
# load(Specs_File)
for (i_exp in 1:dim(arguments)[1])
{
    M <-arguments[i_exp,1]
    Kcol_min <- arguments[i_exp,3]
    Kcol_max <- arguments[i_exp,4]
    Nrow_min <- arguments[i_exp,5]
    Nrow_max <- arguments[i_exp,6]
    range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
    range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
    
    for (j_global in 1:M)
    {
        
        # This reset the sed, that was previously set to 12345 in order to get the same validation subset selected
        set.seed(j_global+i_exp)
        
        # This step defines and samples the subsets of features for training based on the Kcol sample
        Kcol <- round(dim(Xtemp)[2]*(runif(1,Kcol_min/100,Kcol_max/100))) # sample a value from a uniform distribution within Kcol_min and Kcol_max [number of features/columns of the big dataset]
        k <- sample(1:dim(Xtemp)[2],Kcol)
        
        # This step defines and samples the correct subsets of subjects for training
        Nrow <- round(dim(Xtemp[-q,])[1]*(runif(1,Nrow_min/100,Nrow_max/100))) # sample a value from a uniform distribution Nrox_min and Nrow_max [number of rows/subjects of the big dataset]
        n_all <- 1:length(Ytemp)
        n_sub <- n_all[-q]
        a0 = round(Nrow/2) # balanced # of subjects
        # Randomly select patients for prediction in a balanced way based on the Nrow sample and a0
        n1 = sample(which(Ytemp[n_sub]==1),a0)
        n2 = sample(which(Ytemp[n_sub]==0),a0)
        n <- c(n1 , n2)
        
        # Pass the missing value %
        # For real datasets, there's no need to pass the missValperc, because 
        # we will impute whatever the missing values are
        misValperc <- arguments[i_exp,2]
        print(dim(Xtemp))
        print(dim(Ytemp))
        #Xtemp_mis <- prodNA(Xtemp, noNA = misValperc/100)
        # Here we pass ONLY the subset of rows/columns [n,k] for imputation and scaling of Xtemp [training/learning]
        # If M subsets have been already generated offline, then a RData workspace will be loaded
        # with the correspondent imputed/normalized X and Xpred
        Xtemp_mis <- Xtemp[n,k]
        # Here we pass ONLY the subset of columns [,k] for imputation and scaling of Xpred [validation/preditcion]
        Xpred_mis <- Xpred[,k]
        # Here I impute the missing data with the function missForest
        Xtemp_imp <- missForest(Xtemp_mis, maxiter = 5)
        # No imputation for the validation set is needed
        Xpred_imp <- Xpred_mis
        
        ## DATA NORMALIZATION of the sampled matrix without Group and Sex
        ## This step can be generalized if the data is formatted RAW,
        # with categorical variables as binary or strings (see commented out example below)
        # a1 = which(names(Xtemp_imp$ximp) == "Group")
        # a2 = which(names(Xtemp_imp$ximp) == "Sex")
        # cont = 1:length(Xtemp_imp$ximp)
        # cont <- cont[-1*c(a1,a2)]
        # # DATA NORMALIZATION if IMPUTATION IS PERFORMED
        #Xnorm_ALL <- as.data.frame(scale(Xtemp_imp$ximp))
        Xtemp_norm <- as.data.frame(scale(Xtemp_imp$ximp))
        Xpred_norm <- as.data.frame(scale(Xpred_imp))
        SL.glmnet.0 <- function(..., alpha = 0){
            SL.glmnet(..., alpha = alpha)
        } # ridge penalty
        
        SL.glmnet.0.25 <- function(..., alpha = 0.25){
            SL.glmnet(..., alpha = alpha)
        }
        
        SL.glmnet.0.50 <- function(..., alpha = 0.50){
            SL.glmnet(..., alpha = alpha)
        }
        
        SL.glmnet.0.75 <- function(..., alpha = 0.75){
            SL.glmnet(..., alpha = alpha)
        }
        
        SL.gam.1<-function(...,control=gam.control(deg.gam=1)){
            SL.gam(...,control=control)
        }
        SL.gam.3<-function(...,control=gam.control(deg.gam=3)){
            SL.gam(...,control=control)
        }
        SL.gam.4<-function(...,control=gam.control(deg.gam=4)){
            SL.gam(...,control=control)
        }
        SL.gam.5<-function(...,control=gam.control(deg.gam=5)){
            SL.gam(...,control=control)
        }
        
        create.SL.glmnet.alpha<-function(...,alpha=c(0.25,0.5,0.75))
        {
            SL.glmnet(..., alpha=alpha)
        }
        
        SL.library <- c("SL.glm",
                        "SL.glmnet","SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75",
                        "SL.svm","SL.randomForest")
        ## INITIALIZATION BEFORE THE SUPERLEARNER LOOP
        print(c(j_global,i_exp))
        
        # Automated labeling of sub-matrices, assigned to X
        X <- as.data.frame(Xtemp_norm)
        Y <- Ytemp[n]
        
        # This step sets the feature and the cases selected at the job j_global, respectively to kj_global and nj_global 
        eval(parse(text=paste0("k",j_global," <- k")))
        eval(parse(text=paste0("n",j_global," <- n")))
        # print(dim(X))
        # print(dim(Y))
        
        
        ## KNOCKOFF FILTER IMPLEMENTATION  
        ## IMPORTANT  --> subjects # >> features # !!!
        ## It creates KO_result_j objects with all the stats, results, FDR proportion,...
        # knockoff.filter(X, Y, fdr = 0.2, statistic = NULL,
        # threshold = c("knockoff", "knockoff+"), knockoffs = c("equicorrelated","sdp"),
        #               normalize = TRUE, randomize = FALSE)
        
        # eval(parse(text=paste0("try(KO_result_",j_global," = knockoff.filter(X,Y,fdr = 0.05))")))
        # eval(parse(text=paste0("ifelse(exists(\"KO_result_",j_global,"\"),KO_selected_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_",j_global,"$selected))),
        #                        KO_selected_",j_global," <- NULL)")))
        
        # This first step checks if the features are larger than the cases. If not, the knockoff won't be applied and the
        # features selected by the knockoff set to NULL
        # This check won't be necessary anymore once we implement the MF-Knockoff
        if (dim(X)[2]<dim(X)[1])
        {
            eval(parse(text=paste0("KO_result_",j_global," = knockoff.filter(X,Y,fdr = 0.05)")))
            eval(parse(text=paste0("KO_selected_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_",j_global,"$selected)))")))
            eval(parse(text=paste0("print(KO_selected_",j_global,")")))
            
        } else {
            eval(parse(text=paste0("print(KO_selected_",j_global,"<-NULL)")))
        }
        
        ## SUPERLEARNER LOOP
        # SUPERLEARNER-SL FUNCTION CALL that generates SL objects
        ## Superlearner Function ##
        SL <- try(SuperLearner(Y , X , 
                               family=binomial(),
                               SL.library=SL.library,
                               method="method.NNLS",
                               verbose = TRUE,
                               control = list(saveFitLibrary = TRUE),
                               cvControl = list(V=10)))
        # eval(parse(text=paste0("SL_",j_global," <- SL")))
        # eval(parse(text=paste0("SL_",j_global)))
        
        # STEP 7 - GENERATING PREDICTIONS ON THE PREDICTION DATASET
        # Generates SL_Pred object using the predict function on the prediction 
        # dataset with the SL object as the predictive model.
        # SL_Pred returns both the SuperLearner predictions ("pred") and 
        # predictions for each algorithm in the library (SL.library above) newX = Xpred_norm[,k],
        #eval(parse(text=paste0("try(SL_Pred_",j_global," <- predict(SL_",j_global,", Xpred_norm[,k]))")))
        try(SL_Pred <- predict(SL, Xpred_norm))
        
        # This checks if the SL_Pred object was successfully generated (i.e., if it exists)
        # If it does not exist, it is set to a double equal to 100
        # eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
        #                    SL_Pred_",j_global," <- 100)")))
        ifelse(exists("SL_Pred"),'OK',
               SL_Pred <- 100)
        
        ## MSE calculation
        sum=0
        for(s in 1:length(Ypred)) {
            ifelse(typeof(SL_Pred) != "double", sum <- sum+sum(Ypred[s] - SL_Pred$pred[s])^2,
                   sum <- SL_Pred)
            # eval(parse(text=paste0("ifelse(typeof(SL_Pred_",j_global,") != \"double\",
            #                            sum <- sum+sum(Ypred[",s,"] - SL_Pred_",j_global,"$pred[",s,"])^2,sum <- SL_Pred_",j_global,")")))
        }
        ## This step makes the final calculation of the MSE and it labels it with a j --> MSE_j
        eval(parse(text=paste0("MSE_",j_global," <- sum/length(Ypred)")))
        eval(parse(text=paste0("SL_Pred_",j_global," <- SL_Pred")))
        
        eval(parse(text=paste0("coef_",j_global,"_1<-coef(SL)[coef(SL)>=0.1]")))
        eval(parse(text=paste0("coef_",j_global,"_2<-coef(SL)[coef(SL)>=0.2]")))
        eval(parse(text=paste0("coef_",j_global,"_3<-coef(SL)[coef(SL)>=0.3]")))
        # GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECT
        #rm(SL)
        nonzero=0;
        
        
        # SAVE THE RDATA WORKSPACE WITH THE ALL DATA
        # eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,Xpred,Ypred,q,
        #                        Xtemp,Ytemp,cols_to_eliminate,original_names_Data,original_names_Xtemp,
        #                        file= \"~/Binomial_5_new_ValidationSets_Specs.RData\")")))
        eval(parse(text=paste0("save(Xpred,label, q, Ypred, M, i_exp, Ytemp, SL_Pred_",j_global,
                               ",nonzero,n",j_global,",k",j_global,",KO_selected_",j_global,",coef_",j_global,"_1",",coef_",j_global,"_2",",coef_",j_global,"_3",",MSE_",j_global,",
                               file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
                               ,range_k,"_Light_",j_global,"_",label,".RData\")")))
        print(parse(text=paste0("save(Xpred,label, q, Ypred, M, i_exp, Ytemp, SL_Pred_",j_global,
                                ",nonzero,n",j_global,",k",j_global,",KO_selected_",j_global,",coef_",j_global,",
                                file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
                                ,range_k,"_Light_",j_global,"_",label,".RData\")")))
    }
}
#eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,file= \"~/temp_data_info_",label,".RData\")")))
#eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,file= \"/",label,"_info_for_consolidation.RData\")")))

Specs_File_for_consolidation = paste0(workspace_directory,"/",label,"_info_for_consolidation.RData")
save(arguments,label,workspace_directory,Xpred,Ypred,q,Xtemp,Ytemp,cols_to_eliminate,
     original_names_Data,original_names_Xtemp,file= Specs_File_for_consolidation)


for (i in i_exp) {
    print(i)
    M <-arguments[i,1]
    misValperc <- arguments[i,2]
    Kcol_min <- arguments[i,3]
    Kcol_max <- arguments[i,4]
    Nrow_min <- arguments[i,5]
    Nrow_max <- arguments[i,6]
    range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
    range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
    print(c(M,misValperc,Kcol_min,Kcol_max,Nrow_min,Nrow_max))
    for (s in 1:M){
        print(s)
        eval(parse(text=paste0("load(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,"_",label,".RData\")")))
    }
    
    ## THIS SAVES THE CONSOLIDATED WORKSPACE FOR EACH EXPERIMENT
    eval(parse(text=paste0("save.image(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))
}

for (i in i_exp) {
    print(i)
    print(workspace_directory)
    print(arguments)
    M <-arguments[i,1]
    misValperc <- arguments[i,2]
    Kcol_min <- arguments[i,3]
    Kcol_max <- arguments[i,4]
    Nrow_min <- arguments[i,5]
    Nrow_max <- arguments[i,6]
    range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
    range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
    
    for (s in 1:M) {
        eval(parse(text=paste0("file.remove(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,"_",label,".RData\")")))
    }
}

#  GENERATING THE ARRAY OF MSE FOR ALL THE M SL OBJECTS
MSE=0;
for (j in 1:M) {
    eval(parse(text=paste0("MSE[j] <- MSE_",j)))
}

# MSE RANKING
#for (s in seq(10,M,M/10)){
s=M;
MSE_temp <- NULL
MSE_sorted_temp <- NULL
MSE_temp <- data.frame(mse=MSE[1:s],k_set=1:s)
MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]
select_temp = MSE_sorted_temp[1:(M),2]

## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
# "top" is defined at the beginning (line 8) and represents the top MSEs to consider for 
# feature mining (ks). Each one will have a set of best features with their relative highest frequencies
top=M;
eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
for (r in 1:top){
    eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$k_set[r],")")))
}

m = matrix(0,length(SL.library),3)
coe = data.frame(num = t(m))
colnames(coe) = names(coef(SL))
rownames(coe) = c("coef_0.1","coef_0.2","coef_0.3")
for (r in select_temp){
    eval(parse(text=paste0("a1 <- coef_",r,"_1")))
    eval(parse(text=paste0("a2 <- coef_",r,"_2")))
    eval(parse(text=paste0("a3 <- coef_",r,"_3")))
    for (i in names(a1)){
        eval(parse(text=paste0("coe$",i, "[1]<-","coe$",i,"[1]+ 1")))
    }
    for (i in names(a2)){
        eval(parse(text=paste0("coe$",i, "[2]<-","coe$",i,"[2]+ 1")))
    }
    for (i in names(a3)){
        eval(parse(text=paste0("coe$",i, "[3]<-","coe$",i,"[3]+ 1")))
    } 
}

coe = data.frame(t(coe))
coe2 = gather(coe,coef,value)
coe2$algorithm = c(SL.library,SL.library,SL.library)
ggplot(coe2, aes(x = algorithm, y  = value, fill = coef)) + geom_histogram(stat = "identity",position="dodge")

rownames(coe)[which(coe==max(coe))]
# Cumulative KNOCKOFF results
KO_sub <- NULL
for (j in 1:M) {
    eval(parse(text=paste0("KO_sub <- c(KO_sub,KO_selected_",j,")")))
}


# GENERATE HISTOGRAM OF THE CUMULATIVE KNOCKOFF RESULTS FOR SINGLE EXPERIMENT
x = KO_sub;
eval(parse(text=paste0("h=hist(x, plot = FALSE ,breaks=seq(min(x)-0.5, max(x)+0.5, by=1))")))
h$density = h$counts/sum(h$counts)*100
title_temp <- c("Knockoff Filter",range_n,range_k)
print(title_temp)
plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,15))

# GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT
eval(parse(text=paste0("x = k_top_",top,"_temp")))
eval(parse(text=paste0("h=hist(k_top_",top,"_temp, plot = FALSE ,breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
h$density = h$counts/sum(h$counts)*100
title_temp <- c("CBDA-SL",range_n,range_k)
print(title_temp)
plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,2.5))

# RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES
BEST=3;
eval(parse(text=paste0("Top_features=sort(table(k_top_",top,"_temp), decreasing = TRUE)")))
Top_features_labels <- names(Xpred)[as.numeric(names(Top_features))]
#Top_features_labels <- names(Xnorm_sub)[as.numeric(names(Top_features))]
eval(parse(text=paste0("Top_",BEST,"_features_labels <- names(Xpred)[as.numeric(names(Top_features)[1:",BEST,"])]")))
eval(parse(text=paste0("print(Top_",BEST,"_features_labels)")))
eval(parse(text=paste0("print(Top_features[1:",BEST,"])")))


#CV Superlearner function application [NOT TESTED YET]
# CV_SL <- try(CV.SuperLearner(Y,
#                              X,
#                              V=10, family=gaussian(),
#                              SL.library=SL.library,
#                              method="method.NNLS",
#                              verbose = TRUE,
#                              control = list(saveFitLibrary = TRUE),
#                              cvControl = list(V=10), saveAll = TRUE));#,
#                              #parallel = 'multicore'));
# 
# eval(parse(text=paste0("CV_SL_",j_global,"_KO <- CV_SL")));
# 
# eval(parse(text=paste0("ifelse(exists(\"CV_SL_",j_global,"_KO\"),'OK',
#                        CV_SL_",j_global,"_KO <- 1)")))
# 
# eval(parse(text=paste0("save(Xnew,Ynew,CV_SL_",j_global,"_KO,k",j_global,",n",j_global,",file= \"",
#                        workspace_directory,"CBDA_CV_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_",j_global,"_KO.RData\")")))