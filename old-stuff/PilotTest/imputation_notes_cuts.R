#Generate 10% missing values at Random 
NeuroIm1.mis <- prodNA(NeuroIm1, noNA = 0.1)
#Check missing values introduced in the data
summary(NeuroIm1.mis)

# mice_plot <- aggr(NeuroIm1.mis, col=c('navyblue','yellow'),
#                  numbers=TRUE, sortVars=TRUE,
#                  labels=names(NeuroIm1.mis), cex.axis=.7,
#                  gap=3, ylab=c("Missing data","Pattern"))

## IMPUTATION METHODS
# Hmisc should be the first choice of missing value imputation followed by missForest and MICE.
# Hmisc automatically recognizes the variables types and uses bootstrap sample and 
# predictive mean matching to impute missing values. There's np need to separate or 
# treat categorical variable. However, missForest can outperform Hmisc if the observed 
# variables supplied contain sufficient information.
## Hmisc
# Hmisc is a multiple purpose package useful for data analysis, high – level graphics,
# imputing missing values, advanced table making, model fitting & diagnostics (linear
# regression, logistic regression & cox regression) etc. Amidst, the wide range of 
# functions contained in this package, it offers 2 powerful functions for imputing 
# missing values. These are impute() and aregImpute(). 
# impute() function simply imputes missing value using user defined statistical method
# (mean, max, mean). It’s default is median. On the other hand, aregImpute() allows mean 
# imputation using additive regression, bootstrapping, and predictive mean matching.
# In bootstrapping, different bootstrap resamples are used for each of multiple imputations.
# Then, a flexible additive model (non parametric regression method) is fitted on samples
# taken with replacements from original data and missing values (acts as dependent variable)
# are predicted using non-missing values (independent variable).
# Then, it uses predictive mean matching (default) to impute missing values. 
# Predictive mean matching works well for continuous and categorical (binary & multi-level)
# without the need for computing residuals and maximum likelihood fit.
# Here are some important highlights of this package:
# 1)   It assumes linearity in the variables being predicted.
# 2) Fisher’s optimum scoring method is used for predicting categorical variables.

#missForest
# missForest is an implementation of random forest algorithm.
# It’s a non parametric imputation method applicable to various variable types. 
# It yield OOB (out of bag) imputation error estimate.
# Moreover, it provides high level of control on imputation process.
# It has options to return OOB separately (for each variable) instead of aggregating over
# the whole data matrix. This helps to look more closely as to how accurately the model
# has imputed values for each variable.
# Since bagging works well on categorical variable too, # we don’t need to remove them here.
# It very well takes care of missing value pertaining to their variable types:

# Impute missing values, using all parameters as default values (except for maxiter, which is 10)
# maxiter: maximum number of iterations to be performed given the stopping criterion is not met beforehand.

NeuroIm1.imp <- missForest(NeuroIm1.mis, maxiter = 5)

#check imputed values
NeuroIm1.imp$ximp

#check imputation error
NeuroIm1.imp$OOBerror

#comparing actual data accuracy
NeuroIm1.err <- mixError(NeuroIm1.imp$ximp, NeuroIm1.mis, NeuroIm1)
NeuroIm1.err
