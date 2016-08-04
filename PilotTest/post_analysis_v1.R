setwd("~/Documents/NIH-grant/SOCR/PilotTest")
#load("CBDA_SL_M10000_miss0_n60_80_k15_30.RData")
#load("CBDA_SL_M10000_miss0_n60_80_k30_50.RData")   
#load("CBDA_SL_M10000_miss10_n60_80_k15_30.RData")   
load("CBDA_SL_M10000_miss10_n60_80_k30_50.RData")   
#load("CBDA_SL_M10000_miss20_n60_80_k15_30.RData")  
#load("CBDA_SL_M10000_miss20_n60_80_k30_50.RData")   
#load("CBDA_SL_M10000_miss30_n60_80_k15_30.RData")   
#load("CBDA_SL_M10000_miss30_n60_80_k30_50.RData")   
#load("CBDA_SL_M10000_miss40_n60_80_k15_30.RData")   
#load("CBDA_SL_M10000_miss40_n60_80_k30_50.RData")

#eval(parse(text=paste0("save.image(\"/home/simeonem/Documents/NIH-grant/SOCR/GITHUB/CBDA_SL_M",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,".RData\")")))
  TOP=1000;
  TOP_INCR=100;
## GENERATE THE TOP 100 RANKINGS (BY 10)
  for (top in seq(TOP_INCR,TOP,TOP_INCR)){
    eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
    for (i in 1:top){
      eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$rank[i],")")))
    }
  }
## GENERATE THE k_top_100 LIST (BY 10)
for (top in seq(TOP_INCR,TOP,TOP_INCR)){
  eval(parse(text=paste0("k_top_",top,"_",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k," <- k_top_10_temp")))
}

# GENERATE HISTOGRAMS OF THE TOP # OF COVARIATES
for (top in seq(TOP_INCR,TOP,TOP_INCR)){
  eval(parse(text=paste0("x = k_top_",top,"_",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k)))
  #h = hist(x)
  eval(parse(text=paste0("h = hist(k_top_",top,"_temp,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
  #eval(parse(text=paste0("hist(k_top_",top,"_temp, breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
  h$density = h$counts/sum(h$counts)*100
  eval(parse(text=paste0("title_temp = \"\nCBDA Features Mining TOP ",top," MSE\n[M = ",M,", Missing Values = ",misValperc*100,
                         ", \nSubject Sample Range = ",range_n,", \nFeatures Sample Range = ",range_k,"\"")))
  plot(h,freq=FALSE,ylab='Density (%)',xlab='Covariate #',main=title_temp)
  #readline("Press <return to continue")
}

 # RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES 
  BEST=6;
  eval(parse(text=paste0("Top_features <- data.frame(table(k_top_",TOP,"_temp))")))
  colnames(Top_features) <- c("Position","Freq")
  Top_features_ranking <- Top_features[order(Top_features$Freq, decreasing = TRUE),]
  a1=Top_features_ranking$Position[1:BEST]
  names(Xnew[a1]) 
  head(Top_features_ranking)
  eval(parse(text=paste0("TOP_",BEST,"_features_",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,"<-names(Xnew[a1])")))

  
  