# This R script contains all code related to question 7 and building decision trees

library("rpart")
library("MASS");

# Load the biopsy data
data("biopsy");
biopsy <- biopsy[,-1];       # remove sample id column
biopsy <- na.omit(biopsy);   # remove rows w/ missing data

# Build the two trees
biopsy.d_tree_gini <- rpart(class ~ ., data=biopsy, method='class', control=rpart.control(maxdepth=2), parms=list(split="gini"));
biopsy.d_tree_entropy <- rpart(class ~ ., data=biopsy, method='class', control=rpart.control(maxdepth=2), parms=list(split="information"));

# Plot the two trees
par(mfrow=c(1,2), xpd=NA);
plot(biopsy.d_tree_gini, uniform=TRUE, main='Split by Gini');
text(biopsy.d_tree_gini, use.n=TRUE);
plot(biopsy.d_tree_entropy, uniform=TRUE, main='Split by Entropy');
text(biopsy.d_tree_entropy, use.n=TRUE);