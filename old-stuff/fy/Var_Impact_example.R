if (!require("devtools")) install.packages("devtools")
devtools::install_github("ck37/varImpact")
library("varImpact")
####################################
# Create test dataset.
set.seed(1)
N <- 200
num_normal <- 7
X <- as.data.frame(matrix(rnorm(N * num_normal), N, num_normal))
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))
# Add some missing data to X so we can test imputation.
for (i in 1:10) X[sample(nrow(X), 1), sample(ncol(X), 1)] <- NA

####################################
# Basic example
vim <- varImpact(Y = Y, data = X) 
vim
vim$results_all
