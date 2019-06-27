## This generates a "light" Binomial dataset 10kx1k
cat("GENERATNG THE BINOMIAL 10K - 1K\n\n")
start_time_total=Sys.time()
for (i in 1:10){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 1000          # number of variables
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  amplitude = 3.5
  nonzero=c(10,100,200,300,400,500,600,700,800,900)
  beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  z = ztemp()
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Binomial_append_10k_1k.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)



## This generates a "light" Binomial dataset 10kx1k WITH signal/noise 2 (instead of 3.5)
cat("GENERATNG THE BINOMIAL 10K - 1K\n\n")
start_time_total=Sys.time()
for (i in 1:10){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 1000          # number of variables
  #X1 <- matrix(rnorm(n*p, mean=0,sd=2.5),nrow=n, ncol=p);
  #X1 <- matrix(rnorm(n*p),nrow=n, ncol=p);
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  #X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  amplitude = 2.0
  #nonzero=c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  nonzero=c(10,100,200,300,400,500,600,700,800,900)
  #nonzero=c(10,20,30,40,50,60,70,80,90,100)
  beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  z = ztemp()
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Binomial_append_10k_1k_2sd.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)


## This generates a "light" Binomial dataset 10kx1k WITH signal/noise 1 (instead of 3.5 r 2.0)
cat("GENERATNG THE BINOMIAL 10K - 1K\n\n")
start_time_total=Sys.time()
for (i in 1:10){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 1000          # number of variables
  #X1 <- matrix(rnorm(n*p, mean=0,sd=2.5),nrow=n, ncol=p);
  #X1 <- matrix(rnorm(n*p),nrow=n, ncol=p);
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  #X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  amplitude = 1.0
  #nonzero=c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  nonzero=c(10,100,200,300,400,500,600,700,800,900)
  #nonzero=c(10,20,30,40,50,60,70,80,90,100)
  beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  z = ztemp()
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Binomial_append_10k_1k_1sd.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)


## This generates a "light" Null dataset 10kx1k
cat("GENERATNG THE NULL 10K - 1K\n\n")
start_time_total=Sys.time()
for (i in 1:10){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 1000          # number of variables
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  Ytemp = rbinom(n,1,0.5)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Null_append_10k_1k.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)


## This generates a "light" Binomial dataset 100kx10k
cat("GENERATNG THE BINOMIAL 100K - 10K\n\n")
start_time_total=Sys.time()
for (i in 1:100){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 10000          # number of variables
  #X1 <- matrix(rnorm(n*p, mean=0,sd=2.5),nrow=n, ncol=p);
  #X1 <- matrix(rnorm(n*p),nrow=n, ncol=p);
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  #X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  amplitude = 3.5
  nonzero=c(100,1000,2000,3000,4000,5000,6000,7000,8000,9000)
  #nonzero=c(100,200,300,400,500,600,700,800,900,1000)
  #nonzero=c(10,20,30,40,50,60,70,80,90,100)
  beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  z = ztemp()
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  #IDs <- as.character(IDs_temp)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Binomial_append_100k_10k.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_10k_1k.txt",
  #            row.names = TRUE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_100k_10k.txt",append = T,
  #            row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="test_append.txt", append = T,sep = ",",
  #            row.names = FALSE,col.names = FALSE, eol = "\r\n" )
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)



## This generates a "light" Binomial dataset 100kx10k with signal/noise = 2 (instead of 3.5)
cat("GENERATNG THE BINOMIAL 100K - 10K\n\n")
start_time_total=Sys.time()
for (i in 1:100){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 10000          # number of variables
  #X1 <- matrix(rnorm(n*p, mean=0,sd=2.5),nrow=n, ncol=p);
  #X1 <- matrix(rnorm(n*p),nrow=n, ncol=p);
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  #X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  amplitude = 2.0
  nonzero=c(100,1000,2000,3000,4000,5000,6000,7000,8000,9000)
  #nonzero=c(100,200,300,400,500,600,700,800,900,1000)
  #nonzero=c(10,20,30,40,50,60,70,80,90,100)
  beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  z = ztemp()
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  #IDs <- as.character(IDs_temp)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Binomial_append_100k_10k_2sd.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_10k_1k.txt",
  #            row.names = TRUE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_100k_10k.txt",append = T,
  #            row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="test_append.txt", append = T,sep = ",",
  #            row.names = FALSE,col.names = FALSE, eol = "\r\n" )
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)


## This generates a "light" Binomial dataset 100kx10k with signal/noise = 1 (instead of 2.0 or 3.5)
cat("GENERATNG THE BINOMIAL 100K - 10K\n\n")
start_time_total=Sys.time()
for (i in 1:100){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 10000          # number of variables
  #X1 <- matrix(rnorm(n*p, mean=0,sd=2.5),nrow=n, ncol=p);
  #X1 <- matrix(rnorm(n*p),nrow=n, ncol=p);
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  #X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  amplitude = 1.0
  nonzero=c(100,1000,2000,3000,4000,5000,6000,7000,8000,9000)
  #nonzero=c(100,200,300,400,500,600,700,800,900,1000)
  #nonzero=c(10,20,30,40,50,60,70,80,90,100)
  beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  z = ztemp()
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  #IDs <- as.character(IDs_temp)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Binomial_append_100k_10k_1sd.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)








## This generates a "light" Binomial dataset 100kx10k
cat("GENERATNG THE BINOMIAL 1million - 10K\n\n")
start_time_total=Sys.time()
for (i in 1:1000){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 10000          # number of variables
  #X1 <- matrix(rnorm(n*p, mean=0,sd=2.5),nrow=n, ncol=p);
  #X1 <- matrix(rnorm(n*p),nrow=n, ncol=p);
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  #X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  amplitude = 2.0
  nonzero=c(100,1000,2000,3000,4000,5000,6000,7000,8000,9000)
  #nonzero=c(100,200,300,400,500,600,700,800,900,1000)
  #nonzero=c(10,20,30,40,50,60,70,80,90,100)
  beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  z = ztemp()
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  #IDs <- as.character(IDs_temp)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Binomial_append_1M_10k_2sd.txt",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_10k_1k.txt",
  #            row.names = TRUE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_1M_10k.txt",append = T,
  #            row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="test_append.txt", append = T,sep = ",",
  #            row.names = FALSE,col.names = FALSE, eol = "\r\n" )
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)


## This generates a "light" Null dataset 100kx10k
cat("GENERATNG THE NULL 100K - 10K\n\n")
start_time_total=Sys.time()
for (i in 1:100){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 1000          # number of observations
  p = 10000          # number of variables
  # X1 <- matrix(rnorm(n*p, mean=0,sd=2.5),nrow=n, ncol=p);
  # X1 <- matrix(rnorm(n*p),nrow=n, ncol=p);
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  # X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  # amplitude = 3.5
  # nonzero=c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  # nonzero=c(100,200,300,400,500,600,700,800,900,1000)
  # nonzero=c(10,20,30,40,50,60,70,80,90,100)
  # beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  # ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  # z = ztemp()
  # pr = 1/(1+exp(-z))         # pass through an inv-logit function
  # Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  Ytemp = rbinom(n,1,0.5)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  #IDs <- as.character(IDs_temp)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Null_append_100k_10k.csv",append = T, eol = "\r\n" ,
              row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_10k_1k.txt",
  #            row.names = TRUE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="Binomial_append_100k_10k.txt",append = T,
  #            row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="test_append.txt", append = T,sep = ",",
  #            row.names = FALSE,col.names = FALSE, eol = "\r\n" )
  end_time=Sys.time()
  print(end_time-start_time)
}
end_time_total=Sys.time()
print(end_time_total-start_time_total)


## This generates a "light" Null dataset 10kx1k
cat("GENERATNG THE NULL 10K - 1K\n\n")
start_time_total=Sys.time()
for (i in 1:2){
  start_time=Sys.time()
  print(i)
  X2 <- NULL
  n = 20          # number of observations
  p = 10          # number of variables
  X1 <- matrix(round(rnorm(n*p),3),nrow = n, ncol = p)
  # X1 <- X1[,sample(ncol(X1))] # shuffles the columns
  # amplitude = 3.5
  # nonzero=c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  # nonzero=c(100,200,300,400,500,600,700,800,900,1000)
  # nonzero=c(10,20,30,40,50,60,70,80,90,100)
  # beta = amplitude * (1:p %in% nonzero)  # setting the nonzero variables to 3.5
  # ztemp <- function() X1 %*% beta + rnorm(n) # linear combination with a bias
  # z = ztemp()
  # pr = 1/(1+exp(-z))         # pass through an inv-logit function
  # Ytemp = rbinom(n,1,pr)    # bernoulli response variable
  Ytemp = rbinom(n,1,0.5)    # bernoulli response variable
  IDs <- NULL
  IDs_temp <- seq((1+n*(i-1)),i*n,1)
  #IDs <- as.character(IDs_temp)
  X2 <- cbind(IDs_temp,Ytemp,X1)
  X2=as.data.frame(X2)
  X2$IDs_temp<-as.character(X2$IDs_temp)
  write.table(X2,file="Test_append.csv",append = T,
              row.names = FALSE,col.names = FALSE,sep = ",")
  #write.table(X2,file="Binomial_append_100k_10k.txt",
  #            row.names = TRUE,col.names = FALSE,sep = ",", eol = "\r\n" ,fileEncoding = "UTF-8")
  #write.table(X2,file="test_append.txt", append = T,sep = ",",
  #            row.names = FALSE,col.names = FALSE, eol = "\r\n" )
  end_time=Sys.time()
  print(end_time-start_time)
}

end_time_total=Sys.time()
print(end_time_total-start_time_total)
