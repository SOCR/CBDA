if (!require("devtools")) install.packages("devtools")
devtools::install_github("ck37/varImpact")
library("varImpact")

curl http://bioconductor.org/packages/3.1/bioc/src/contrib/PACKAGES
download.file(url = "http://bioconductor.org/packages/3.1/bioc/src/contrib/PACKAGES", tempfile())
library("")

source("https://bioconductor.org/biocLite.R")
biocLite("BiocInstaller")
biocLite("BiocGenerics")
biocLite("Biobase")


install.packages("party")

setwd("~/GitHub/CBDA/Pipeline/DISTRIBUTED/BINOMIALdataset/TEST")
library(party)
library(ggplot2)
library(glmnet)

dat = read.csv("Binomial_dataset_1_new_sd.txt", header = T)
dat2 = dat[1:100, ]
dat3 = read.csv("Binomial_dataset_final.txt", header = T)
dat4 = dat3[1:200, ]
dat5 = read.csv("Binomial_dataset_5_final.txt", header = T)
daty = dat$Ytemp
datx = dat[,2:301]
imp = data.frame(1:300, rep(0, 300), rep(0, 300))
colnames(imp) = c("Variable", "Count", "Signal")
imp$Signal[c(1,30,60,100,130,160,200,230,260,300)] = 1
imp$Signal = ifelse(imp$Signal == 1, "Yes", "No")


colnames(imp) = c("Variable", "Count")

full_forest = cforest(Ytemp~., data = dat2)
full_vimp = varimp(full_forest)

full_df = data.frame(1:300, full_vimp, rep(0, 300))
colnames(full_df) = c("Variable", "Importance", "Signal")
full_df$Signal[c(1,30,60,100,130,160,200,230,260,300)] = 1
full_df$Signal = ifelse(full_df$Signal == 1, "Yes", "No")
full_df$Signal = factor(full_df$Signal)

full_df = full_df[order(full_df$Importance, decreasing=T), ]

fulltoplot = full_df[1:20,]

full_plot = ggplot(fulltoplot, aes(x = as.character(fulltoplot$Variable), y = Importance, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Importance") + 
  ggtitle("Random Forest Variable Importance")
full_plot


c(1,30,60,100,130,160,200,230,260,300)

k = 1
vars = 1:300
while (k < 150){
  select = sample(1:300, 100, replace = F)
  xtemp = datx[select,]
  ytemp = daty[select]
  tempdat = cbind(xtemp, ytemp)
  temp_forest = cforest(ytemp ~., data = tempdat)
  temp_vimp = varimp(temp_forest)
  temp_df = data.frame(vars, temp_vimp)
  temp_df = temp_df[order(temp_vimp, decreasing = T)[1:20], ]
  imp[temp_df$vars, "Count"] = imp[temp_df$vars, "Count"] + 1
  
  if(k%%50 == 0) print(k)
  k = k + 1
}

toplot = imp[order(imp$Count, decreasing = T)[1:20], ]
colnames(toplot) = c("Variable", "Count", "Signal")



implot = ggplot(toplot, aes(x = as.character(toplot$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("Random Forest Variable Importance")
implot

save.image("rf_varimport.Rdata")

vimpact = varImpact(Y = daty, data = datx)
vimpactmat = as.matrix(vimpact$results_all)

vimpactsort = as.data.frame(vimpactmat)
vsort = vimpactsort[1:20, ]
vsort$"Variable" = as.numeric(c("130", "160", "129", "297", "1", "83", "260", "225", "145", "148", 
                     "198", "178", "25", "287", "17", "65", "190", "37", "200", "262"))
vsort$"Signal" = factor(c(1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0))
vplot = ggplot(vsort, aes(x = as.character(vsort$Variable), y = Estimate, fill = Signal)) + geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Importance") + ggtitle("varImpact Variable Importance") + 
  theme(axis.text.x=element_blank())
vplot

save.image("varImpact.Rdata")
c(1,30,60,100,130,160,200,230,260,300)


same = c()
z = 1
for (elem in imp$Variable){
  if(k %in% vsort$Variable){
    same[z] = k
    z = z + 1
  } 
}



, control = cforest_unbiased(mtry = 10)