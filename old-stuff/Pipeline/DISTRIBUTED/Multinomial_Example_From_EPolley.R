## multi-class classification
library(SuperLearner)
set.seed(843)
N <- 100

# outcome
Y <- sample(c("A", "B", "C"), size = N, replace = TRUE, prob = c(.1, .5, .4))

# variables
X1 <- rnorm(n = N, mean = (as.numeric(Y == "A") + .5*(as.numeric(Y == "C"))), sd = 1)
X2 <- rnorm(n = N, mean = (as.numeric(Y == "B")), sd = 1)
X3 <- rnorm(n = N, mean = (-1*as.numeric(Y == "B" | Y == "C")), sd = 1)
X4 <- rnorm(n = N, mean = X2, sd = 1)
X5 <- rnorm(n = N, mean = (X1*as.numeric(Y == "A") + as.numeric(Y == "A" | Y == "C")), sd = 1)

DAT <- data.frame(X1, X2, X3, X4, X5)


# test Data
# outcome
M <- 10000
Y_test <- sample(c("A", "B", "C"), size = M, replace = TRUE, prob = c(.1, .5, .4))

# variables
X1_test <- rnorm(n = M, mean = (as.numeric(Y_test == "A") + .5*(as.numeric(Y_test == "C"))), sd = 1)
X2_test <- rnorm(n = M, mean = (as.numeric(Y_test == "B")), sd = 1)
X3_test <- rnorm(n = M, mean = (-1*as.numeric(Y_test == "B" | Y_test == "C")), sd = 1)
X4_test <- rnorm(n = M, mean = X2_test, sd = 1)
X5_test <- rnorm(n = M, mean = (X1_test*as.numeric(Y_test == "A") + as.numeric(Y_test == "A" | Y_test == "C")), sd = 1)

DAT_test <- data.frame(X1 = X1_test, X2 = X2_test, X3 = X3_test, X4 = X4_test, X5 = X5_test)

figure
library(GGally)
DAT2 <- data.frame(Y, DAT)
ggpairs(DAT2, color = "Y")

# create the 3 binary variables
Y_A <- as.numeric(Y == "A")
Y_B <- as.numeric(Y == "B")
Y_C <- as.numeric(Y == "C")

# simple library, should include more classifiers
SL.library <- c("SL.gbm", "SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean")

# least squares loss function
fit_A <- SuperLearner(Y = Y_A, X = DAT, newX = DAT_test, SL.library = SL.library, verbose = FALSE, method = "method.NNLS", family = binomial(), cvControl = list(stratifyCV = TRUE))
fit_B <- SuperLearner(Y = Y_B, X = DAT, newX = DAT_test, SL.library = SL.library, verbose = FALSE, method = "method.NNLS", family = binomial(), cvControl = list(stratifyCV = TRUE))
fit_C <- SuperLearner(Y = Y_C, X = DAT, newX = DAT_test, SL.library = SL.library, verbose = FALSE, method = "method.NNLS", family = binomial(), cvControl = list(stratifyCV = TRUE))

SL_pred <- data.frame(pred_A = fit_A$SL.predict[, 1], pred_B = fit_B$SL.predict[, 1], pred_C = fit_C$SL.predict[, 1])
Classify <- apply(SL_pred, 1, function(xx) c("A", "B", "C")[unname(which.max(xx))])
table(Classify, Y_test)