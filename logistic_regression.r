library("glmnet")

# Import heart disease data
heart_disease <- read.csv("~/Downloads/heart_disease.csv")

# Delete the empty column
heart_disease$X <- NULL

# Convert categorical predictor(s) to dummy variables
heart_disease$CHD <- as.numeric(ifelse(heart_disease$CHD == "Present", 1, 0))

# Run the logistic regression
heart_disease.log_reg = glm(formula=heart_disease$CHD ~ heart_disease$SBP + heart_disease$TOBACCO + heart_disease$LDL + heart_disease$ADIPOSITY + heart_disease$FAMHIST + heart_disease$TYPEA + heart_disease$OBESITY + heart_disease$ALCOHOL + heart_disease$AGE, family=binomial(logit), data=heart_disease)

# View the summary
summary(heart_disease.log_reg)

# View the odds of heart disease
exp(cbind(OR = coef(heart_disease.log_reg), confint(heart_disease.log_reg)))

# Import second data set and change to drop statistically insignificant attributes
heart_disease_reduced <- read.csv("~/Downloads/heart_disease.csv")
heart_disease_reduced$X <- NULL
heart_disease_reduced$CHD <- as.numeric(ifelse(heart_disease_reduced$CHD == "Present", 1, 0))
heart_disease_reduced$ALCOHOL <- NULL
heart_disease_reduced$SBP <- NULL

# Run the logistic regression
heart_disease_reduced.log_reg = glm(formula=heart_disease_reduced$CHD ~ heart_disease_reduced$TOBACCO + heart_disease_reduced$LDL + heart_disease_reduced$ADIPOSITY + heart_disease_reduced$FAMHIST + heart_disease_reduced$TYPEA + heart_disease_reduced$OBESITY + heart_disease_reduced$AGE, family=binomial(logit), data=heart_disease_reduced)

# View the summary
summary(heart_disease_reduced.log_reg)

# View the odds of heart disease
exp(cbind(OR = coef(heart_disease_reduced.log_reg), confint(heart_disease_reduced.log_reg)))

# Convert both data frames to matrices for cross fold validation
heart_disease.matrix <- model.matrix(~. - 1, data=heart_disease[,-10])
heart_disease_reduced.matrix <- model.matrix(~. - 1, data=heart_disease_reduced[,-8])

# Cross Validation on full data set
heart_disease.five_fold_cv <- cv.glmnet(x=heart_disease.matrix, y=heart_disease$CHD, nfolds=5)

# Cross Validation on reduced data set
heart_disease_reduced.five_fold_cv <- cv.glmnet(x=heart_disease_reduced.matrix, y=heart_disease_reduced$CHD, nfolds=5)