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
library(party)
setwd("~/GitHub/CBDA/Pipeline/DISTRIBUTED/BINOMIALdataset/TEST")
library(party)
library(ggplot2)

dat1 = read.csv("Binomial_dataset_1_new_sd.txt", header = T)
dat2 = read.csv("Binomial_dataset_final.txt", header = T)
dat3 = read.csv("Binomial_dataset_5_final.txt", header = T)

colnames(dat1) = c("Ytemp", as.character(1:300))
colnames(dat2) = c("Ytemp", as.character(1:100))
colnames(dat3) = c("Ytemp", as.character(1:1500))

x1 = dat1[,2:301]
x2 = dat2[,2:101]
x3 = dat3[,2:1501]

y1 = dat1[, 1]
y2 = dat2[, 1]
y3 = dat3[, 1]

imp1 = data.frame(1:300, rep(0, 300), rep(0, 300))
imp2 = data.frame(1:100, rep(0, 100), rep(0, 100))
imp3 = data.frame(1:1500, rep(0, 1500), rep(0, 1500))

colnames(imp1) = c("Variable", "Count", "Signal")
colnames(imp2) = c("Variable", "Count", "Signal")
colnames(imp3) = c("Variable", "Count", "Signal")

imp1$Signal[c(1,30,60,100,130,160,200,230,260,300)] = 1
imp2$Signal[c(10,20,30,40,50,60,70,80,90,100)] = 1
imp3$Signal[c(1,100,200,400,600,800,1000,1200,1400,1500)] = 1

imp1$Signal = ifelse(imp1$Signal == 1, "Yes", "No")
imp2$Signal = ifelse(imp2$Signal == 1, "Yes", "No")
imp3$Signal = ifelse(imp3$Signal == 1, "Yes", "No")


## experiment 1: all rows, all columns
imp1 = data.frame(1:300, rep(0, 300), rep(0, 300))
imp2 = data.frame(1:100, rep(0, 100), rep(0, 100))
imp3 = data.frame(1:1500, rep(0, 1500), rep(0, 1500))

colnames(imp1) = c("Variable", "Importance", "Signal")
colnames(imp2) = c("Variable", "Importance", "Signal")
colnames(imp3) = c("Variable", "Importance", "Signal")

imp1$Signal[c(1,30,60,100,130,160,200,230,260,300)] = 1
imp2$Signal[c(10,20,30,40,50,60,70,80,90,100)] = 1
imp3$Signal[c(1,100,200,400,600,800,1000,1200,1400,1500)] = 1

imp1$Signal = ifelse(imp1$Signal == 1, "Yes", "No")
imp2$Signal = ifelse(imp2$Signal == 1, "Yes", "No")
imp3$Signal = ifelse(imp3$Signal == 1, "Yes", "No")

forest1 = cforest(Ytemp~., data = dat1)
forest2 = cforest(Ytemp~., data = dat2)
forest3 = cforest(Ytemp~., data = dat3)

imp1$Importance = varimp(forest1)
imp2$Importance = varimp(forest2)
imp3$Importance = varimp(forest3)

imp1 = imp1[order(imp1$Importance, decreasing = T)[1:20], ]
imp2 = imp2[order(imp2$Importance, decreasing = T)[1:20], ]
imp3 = imp3[order(imp3$Importance, decreasing = T)[1:20], ]

exp1_1 = imp1
exp1_2 = imp2
exp1_3 = imp3

### experiment 2: 5-15%
p1 = ncol(x1)
p2 = ncol(x2)
p3 = ncol(x3)

n1 = nrow(x1)
n2 = nrow(x2)
n3 = nrow(x3)

vars1 = 1:300
vars2 = 1:100
vars3 = 1:1500

lb1 = floor(p1 * .05)
ub1 = floor(p1 * .15)
lb2 = floor(p2 * .05)
ub2 = floor(p2 * .15)
lb3 = floor(p3 * .05)
ub3 = floor(p3 * .15)

k = 1
while (k < 1000){
  s1 = sample(lb1:ub1, 1)
  s2 = sample(lb2:ub2, 1)
  s3 = sample(lb3:ub3, 1)
  
  select1 = sample(1:p1, s1)
  select2 = sample(1:p2, s2)
  select3 = sample(1:p3, s3)
  
  xtemp1 = x1[, select1]
  xtemp2 = x2[, select2]
  xtemp3 = x3[, select3]
  
  d1 = cbind(y1, xtemp1)
  d2 = cbind(y2, xtemp2)
  d3 = cbind(y3, xtemp3)
  
  forest1 = cforest(y1~., data = d1)
  forest2 = cforest(y2~., data = d2)
  forest3 = cforest(y3~., data = d3)
  
  vimp1 = varimp(forest1)
  vimp2 = varimp(forest2)
  vimp3 = varimp(forest3)
  
  temp1 = data.frame(select1, vimp1)
  temp2 = data.frame(select2, vimp2)
  temp3 = data.frame(select3, vimp3)
  
  temp1 = temp1[order(vimp1, decreasing = T)[1:lb1], ]
  temp2 = temp2[order(vimp2, decreasing = T)[1:lb2], ]
  temp3 = temp3[order(vimp3, decreasing = T)[1:lb3], ]
  
  imp1[temp1$select1, "Count"] = imp1[temp1$select1, "Count"] + 1
  imp2[temp2$select2, "Count"] = imp2[temp2$select2, "Count"] + 1
  imp3[temp3$select3, "Count"] = imp3[temp3$select3, "Count"] + 1
  
  if(k%%50 == 0) print(k)
  k = k + 1
}

exp2_1a = imp1
exp2_2a = imp2
exp2_3a = imp3

### experiment 2: 15-30%
lb1 = floor(p1 * .15)
ub1 = floor(p1 * .30)
lb2 = floor(p2 * .15)
ub2 = floor(p2 * .30)
lb3 = floor(p3 * .15)
ub3 = floor(p3 * .30)

k = 1
while (k < 1000){
  s1 = sample(lb1:ub1, 1)
  s2 = sample(lb2:ub2, 1)
  s3 = sample(lb3:ub3, 1)
  
  select1 = sample(1:p1, s1)
  select2 = sample(1:p2, s2)
  select3 = sample(1:p3, s3)
  
  xtemp1 = x1[, order(select1)]
  xtemp2 = x2[, order(select2)]
  xtemp3 = x3[, order(select3)]
  
  d1 = cbind(y1, xtemp1)
  d2 = cbind(y2, xtemp2)
  d3 = cbind(y3, xtemp3)
  
  forest1 = cforest(y1~., data = d1)
  forest2 = cforest(y2~., data = d2)
  forest3 = cforest(y3~., data = d3)
  
  vimp1 = varimp(forest1)
  vimp2 = varimp(forest2)
  vimp3 = varimp(forest3)
  
  temp1 = data.frame(select1, vimp1)
  temp2 = data.frame(select2, vimp2)
  temp3 = data.frame(select3, vimp3)
  
  temp1 = temp1[order(vimp1, decreasing = T)[1:lb1], ]
  temp2 = temp2[order(vimp2, decreasing = T)[1:lb2], ]
  temp3 = temp3[order(vimp3, decreasing = T)[1:lb3], ]
  
  imp1[temp1$select1, "Count"] = imp1[temp1$select1, "Count"] + 1
  imp2[temp2$select2, "Count"] = imp2[temp2$select2, "Count"] + 1
  imp3[temp3$select3, "Count"] = imp3[temp3$select3, "Count"] + 1
  
  if(k%%50 == 0) print(k)
  k = k + 1
}

exp2_1b = imp1
exp2_2b = imp2
exp2_3b = imp3

save.image("RFvarimp.Rdata")

### experiment 3: 30-60%
lb1 = floor(n1 * .30)
ub1 = floor(n1 * .60)
lb2 = floor(n2 * .30)
ub2 = floor(n2 * .60)
lb3 = floor(n3 * .30)
ub3 = floor(n3 * .60)

k = 1
while (k < 1000){
  s1 = sample(lb1:ub1, 1)
  s2 = sample(lb2:ub2, 1)
  s3 = sample(lb3:ub3, 1)
  
  select1 = sample(1:n1, s1)
  select2 = sample(1:n2, s2)
  select3 = sample(1:n3, s3)
  
  xtemp1 = x1[order(select1), ]
  xtemp2 = x2[order(select2), ]
  xtemp3 = x3[order(select3), ]
  
  ytemp1 = y1[order(select1)]
  ytemp2 = y2[order(select2)]
  ytemp3 = y3[order(select3)]
  
  d1 = cbind(ytemp1, xtemp1)
  d2 = cbind(ytemp2, xtemp2)
  d3 = cbind(ytemp3, xtemp3)
  
  forest1 = cforest(ytemp1~., data = d1)
  forest2 = cforest(ytemp2~., data = d2)
  forest3 = cforest(ytemp3~., data = d3)
  
  vimp1 = varimp(forest1)
  vimp2 = varimp(forest2)
  vimp3 = varimp(forest3)
  
  temp1 = data.frame(vars1, vimp1)
  temp2 = data.frame(vars2, vimp2)
  temp3 = data.frame(vars3, vimp3)
  
  temp1 = temp1[order(vimp1, decreasing = T)[1:lb1], ]
  temp2 = temp2[order(vimp2, decreasing = T)[1:lb2], ]
  temp3 = temp3[order(vimp3, decreasing = T)[1:lb3], ]
  
  imp1[temp1$vars1, "Count"] = imp1[temp1$vars1, "Count"] + 1
  imp2[temp2$vars2, "Count"] = imp2[temp2$vars2, "Count"] + 1
  imp3[temp3$vars3, "Count"] = imp3[temp3$vars3, "Count"] + 1
  
  
  if(k%%50 == 0) print(k)
  # print(k)
  k = k + 1
}

exp3_1a = imp1
exp3_2a = imp2
exp3_3a = imp3

### experiment 3: 60-80%
lb1 = floor(n1 * .60)
ub1 = floor(n1 * .80)
lb2 = floor(n2 * .60)
ub2 = floor(n2 * .80)
lb3 = floor(n3 * .60)
ub3 = floor(n3 * .80)

k = 1
while (k < 1000){
  s1 = sample(lb1:ub1, 1)
  s2 = sample(lb2:ub2, 1)
  s3 = sample(lb3:ub3, 1)
  
  select1 = sample(1:n1, s1)
  select2 = sample(1:n2, s2)
  select3 = sample(1:n3, s3)
  
  xtemp1 = x1[order(select1), ]
  xtemp2 = x2[order(select2), ]
  xtemp3 = x3[order(select3), ]
  
  ytemp1 = y1[order(select1)]
  ytemp2 = y2[order(select2)]
  ytemp3 = y3[order(select3)]
  
  d1 = cbind(ytemp1, xtemp1)
  d2 = cbind(ytemp2, xtemp2)
  d3 = cbind(ytemp3, xtemp3)
  
  forest1 = cforest(ytemp1~., data = d1)
  forest2 = cforest(ytemp2~., data = d2)
  forest3 = cforest(ytemp3~., data = d3)
  
  vimp1 = varimp(forest1)
  vimp2 = varimp(forest2)
  vimp3 = varimp(forest3)
  
  temp1 = data.frame(vars1, vimp1)
  temp2 = data.frame(vars2, vimp2)
  temp3 = data.frame(vars3, vimp3)
  
  temp1 = temp1[order(vimp1, decreasing = T)[1:20], ]
  temp2 = temp2[order(vimp2, decreasing = T)[1:10], ]
  temp3 = temp3[order(vimp3, decreasing = T)[1:50], ]
  
  imp1[temp1$vars1, "Count"] = imp1[temp1$vars1, "Count"] + 1
  imp2[temp2$vars2, "Count"] = imp2[temp2$vars2, "Count"] + 1
  imp3[temp3$vars3, "Count"] = imp3[temp3$vars3, "Count"] + 1
  
  
  if(k%%50 == 0) print(k)
  # print(k)
  k = k + 1
}

exp3_1b = imp1
exp3_2b = imp2
exp3_3b = imp3

## Experiment 1 Plots
exp1_1.source = exp1_1
exp1_2.source = exp1_2
exp1_3.source = exp1_3

exp1_1.plot = ggplot(exp1_1.source, aes(x = as.character(exp1_1.source$Variable), y = Importance, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Importance") + 
  ggtitle("300x300, All rows/columns") + 
  theme(axis.text.x=element_blank())
exp1_1.plot

exp1_2.plot = ggplot(exp1_2.source, aes(x = as.character(exp1_2.source$Variable), y = Importance, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Importance") + 
  ggtitle("300x100, All rows/columns")+ 
  theme(axis.text.x=element_blank())
exp1_2.plot

exp1_3.plot = ggplot(exp1_3.source, aes(x = as.character(exp1_3.source$Variable), y = Importance, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Importance") + 
  ggtitle("300x1500, All rows/columns")+ 
  theme(axis.text.x=element_blank())
exp1_3.plot


## Experiment 2, part a Plots
exp2_1a.source = exp2_1a[order(exp2_1a$Count, decreasing = T)[1:20], ]
exp2_2a.source = exp2_2a[order(exp2_2a$Count, decreasing = T)[1:20], ]
exp2_3a.source = exp2_3a[order(exp2_3a$Count, decreasing = T)[1:20], ]

exp2_1a.plot = ggplot(exp2_1a.source, aes(x = as.character(exp2_1a.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x300, 5-15% Columns")+ 
  theme(axis.text.x=element_blank())
exp2_1a.plot

exp2_2a.plot = ggplot(exp2_2a.source, aes(x = as.character(exp2_2a.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x100, 5-15% Columns")+ 
  theme(axis.text.x=element_blank())
exp2_2a.plot

exp2_3a.plot = ggplot(exp2_3a.source, aes(x = as.character(exp2_3a.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x1500, 5-15% Columns")+ 
  theme(axis.text.x=element_blank())
exp2_3a.plot

## Experiment 2, part b plots
exp2_1b.source = exp2_1b[order(exp2_1b$Count, decreasing = T)[1:20], ]
exp2_2b.source = exp2_2b[order(exp2_2b$Count, decreasing = T)[1:20], ]
exp2_3b.source = exp2_3b[order(exp2_3b$Count, decreasing = T)[1:20], ]

exp2_1b.plot = ggplot(exp2_1b.source, aes(x = as.character(exp2_1b.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x300, 15-30% Columns")+ 
  theme(axis.text.x=element_blank())
exp2_1b.plot

exp2_2b.plot = ggplot(exp2_2b.source, aes(x = as.character(exp2_2b.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x100, 15-30% Columns")+ 
  theme(axis.text.x=element_blank())
exp2_2b.plot

exp2_3b.plot = ggplot(exp2_3b.source, aes(x = as.character(exp2_3b.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x1500, 15-30% Columns")+ 
  theme(axis.text.x=element_blank())
exp2_3b.plot

## Experiment 3, part a plots
exp3_1a.source = exp3_1a[order(exp3_1a$Count, decreasing = T)[1:20], ]
exp3_2a.source = exp3_2a[order(exp3_2a$Count, decreasing = T)[1:20], ]
exp3_3a.source = exp3_3a[order(exp3_3a$Count, decreasing = T)[1:20], ]

exp3_1a.plot = ggplot(exp3_1a.source, aes(x = as.character(exp3_1a.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x300, 30-60% Rows")+ 
  theme(axis.text.x=element_blank())
exp3_1a.plot

exp3_2a.plot = ggplot(exp3_2a.source, aes(x = as.character(exp3_2a.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x100, 30-60% Rows")+ 
  theme(axis.text.x=element_blank())
exp3_2a.plot

exp3_3a.plot = ggplot(exp3_3a.source, aes(x = as.character(exp3_3a.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x1500, 30-60% Rows")+ 
  theme(axis.text.x=element_blank())
exp3_3a.plot

## Experiment 3, part b plots
exp3_1b.source = exp3_1b[order(exp3_1b$Count, decreasing = T)[1:20], ]
exp3_2b.source = exp3_2b[order(exp3_2b$Count, decreasing = T)[1:20], ]
exp3_3b.source = exp3_3b[order(exp3_3b$Count, decreasing = T)[1:20], ]

exp3_1b.plot = ggplot(exp3_1b.source, aes(x = as.character(exp3_1b.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x300, 60-80% Rows")+ 
  theme(axis.text.x=element_blank())
exp3_1b.plot

exp3_2b.plot = ggplot(exp3_2b.source, aes(x = as.character(exp3_2b.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x100, 60-80% Rows")+ 
  theme(axis.text.x=element_blank())
exp3_2b.plot

exp3_3b.plot = ggplot(exp3_3b.source, aes(x = as.character(exp3_3b.source$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("300x1500, 60-80% Rows")+ 
  theme(axis.text.x=element_blank())
exp3_3b.plot

save.image("RFvarimp.Rdata")


toplot = imp1[order(imp1$Count, decreasing = T)[1:20], ]
colnames(toplot) = c("Variable", "Count", "Signal")


full_plot = ggplot(fulltoplot, aes(x = as.character(fulltoplot$Variable), y = Importance, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Importance") + 
  ggtitle("Random Forest Variable Importance")
full_plot


implot = ggplot(toplot, aes(x = as.character(toplot$Variable), y = Count, fill = Signal)) + 
  geom_col(width = .5) + coord_flip() + 
  xlab("Variable") + ylab("Count") + 
  ggtitle("Random Forest Variable Importance")
implot

save.image("rf_varimport.Rdata")




full_forest = cforest(Ytemp~., data = dat2)
full_vimp = varimp(full_forest)

full_df = data.frame(1:300, full_vimp, rep(0, 300))
colnames(full_df) = c("Variable", "Importance", "Signal")
full_df$Signal[c(1,30,60,100,130,160,200,230,260,300)] = 1
full_df$Signal = ifelse(full_df$Signal == 1, "Yes", "No")
full_df$Signal = factor(full_df$Signal)

full_df = full_df[order(full_df$Importance, decreasing=T), ]

fulltoplot = full_df[1:20,]





### nah uh
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