# This plots the MEAN SuperLearner coefficients values for each algorithm used in the SL.library
# across the total runs (i.e., M for the TRAINING step, ~50-100 for the VALIDATION step)
barplot(colMeans(m_validation))
barplot(colMeans(m_training))
# This plots the MEDIAN SuperLearner coefficient values for each algorithm used in the SL.library
# the SL.library across the total runs (i.e., M for the TRAINING step, ~50-100 for VALIDATION step)
barplot(apply(m_validation, 2, median))
barplot(apply(m_training, 2, median))


## TRAINING STEP
# This lists the algorithms with positive median values for the SuperLearner coefficients
algorithm_list[which(apply(m_training, 2, median)>0)]
# This lists the algorithms with mean values for the SuperLearner coefficients greater than threshold
threshold=0.05
algorithm_list[which(apply(m_training, 2, mean)>threshold)]

## VALIDATION STEP
# This lists the algorithms with positive median values for the SuperLearner coefficients
CBDA_object$SuperLearnerLibrary[which(apply(m_validation, 2, median)>0)]
# This lists the algorithms with mean values for the SuperLearner coefficients greater than threshold
threshold=0.05
CBDA_object$SuperLearnerLibrary[which(apply(m_validation, 2, mean)>threshold)]
