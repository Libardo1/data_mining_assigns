library("glmnet")
library("ROCR")

# Load Spam data
load("spam.Rdata")
spam$spam <- as.numeric(ifelse(spam$spam == "1", 1, 0))
spam$test <- as.numeric(ifelse(spam$test == "TRUE", 1, 0))

# Removing rows without data
normalized_spam <- model.matrix(~. - 1, data=spam)
log_transform_spam <- model.matrix(~. - 1, data=spam)
binarized_spam <- model.matrix(~. - 1, data=spam)

# Normalizing the spam data
for (i in seq(from=1, to=ncol(spam), by=1))
  for(j in seq(from=1, to=nrow(spam), by=1))
    normalized_spam[j,i] <- (spam[j,i] - mean(spam[,i])) / sd(spam[,i]);

# Log transform all the data
for (i in seq(from=1, to=nrow(spam), by=1))
  for(j in seq(from=1, to=ncol(spam), by=1))
    log_transform_spam[i, j] <- log(spam[i,j] + 0.1)

# Binarize all the data
for (i in seq(from=1, to=nrow(spam), by=1))
  for(j in seq(from=1, to=ncol(spam), by=1))
    binarized_spam[i, j] <- ifelse(spam[i,j] > 0, 1, 0)

heart_disease.matrix <- model.matrix(~. - 1, data=spam[,-10])

# Run cross validation on data
normalized_spam.cv <- cv.glmnet(x=normalized_spam[, -(58:59)], y=normalized_spam[, 58])
log_transform_spam.cv <- cv.glmnet(x=log_transform_spam[, -(58:59)], y=log_transform_spam[, 58])
binarized_spam.cv <- cv.glmnet(x=binarized_spam[, -(58:59)], y=binarized_spam[, 58])

# Run Logistic Regression
normalized_spam.log_reg = glm(formula=spam[, 58] ~ normalized_spam[, 1] + normalized_spam[, 2] + normalized_spam[, 3] + normalized_spam[, 4] + normalized_spam[, 5] + normalized_spam[, 6] + normalized_spam[, 7] + normalized_spam[, 8] + normalized_spam[, 9] + normalized_spam[, 10] + normalized_spam[, 11] + normalized_spam[, 12] + normalized_spam[, 13] + normalized_spam[, 14] + normalized_spam[, 15] + normalized_spam[, 16] + normalized_spam[, 17] + normalized_spam[, 18] + normalized_spam[, 19] + normalized_spam[, 20] + normalized_spam[, 21] + normalized_spam[, 22] + normalized_spam[, 23] + normalized_spam[, 24] + normalized_spam[, 25] + normalized_spam[, 26] + normalized_spam[, 27] + normalized_spam[, 28] + normalized_spam[, 29] + normalized_spam[, 30] + normalized_spam[, 31] + normalized_spam[, 32] + normalized_spam[, 33] + normalized_spam[, 34] + normalized_spam[, 35] + normalized_spam[, 36] + normalized_spam[, 37] + normalized_spam[, 38] + normalized_spam[, 39] + normalized_spam[, 40] + normalized_spam[, 41] + normalized_spam[, 42] + normalized_spam[, 43] + normalized_spam[, 44] + normalized_spam[, 45] + normalized_spam[, 46] + normalized_spam[, 47] + normalized_spam[, 48] + normalized_spam[, 49] + normalized_spam[, 50] + normalized_spam[, 51] + normalized_spam[, 52] + normalized_spam[, 53] + normalized_spam[, 54] + normalized_spam[, 55] + normalized_spam[, 56], family=binomial(logit))