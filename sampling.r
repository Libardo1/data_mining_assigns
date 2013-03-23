#Initialize fields
library(ggplot2)
bostonHousing = read.table(file="boston-housing.txt", header=TRUE, sep="")
sampleMeans = c(0, 0, 0, 0, 0);
sampleDifferences = c(0, 0, 0, 0, 0);
medv_vec = bostonHousing$MEDV
medvMean = mean(bostonHousing$MEDV)

#Simple Random Sampling
cat("-------Calculating Mean with Simple Random Samples-----\n");
for (i in seq(from=1, to=5, by=1)) {
  sampleMeans[i] = mean(sample(bostonHousing$MEDV, size=50, replace=FALSE));
  sampleDifferences[i] = sampleMeans[i] - medvMean
  cat(paste("\tMean for sample ", c(i), ": ", c(sampleMeans[i]), "\n"));
  cat(paste("\tDifference from mean: ", c(sampleDifferences[i]), "\n"))
}

strataMeans = c(0, 0, 0, 0, 0);
strataDifferences = c(0, 0, 0, 0, 0);

#Strata Sampling
cat("\n-------Calculating Mean with Strata Samples-----\n");
for (i in seq(from=1, to=5, by=1)) {
  bostonHousing.strata <- strata(bostonHousing, stratanames=c("CHAS"), size=c(25, 25), method="srswor");
  strataMeans[i]= mean(medv_vec[c(bostonHousing.strata$ID_unit)]);
  strataDifferences[i] = strataMeans[i] - medvMean
  cat(paste("\tMean for strata sample ", c(i), ": ", c(strataMeans[i]), "\n"));
  cat(paste("\tStrata Difference from mean: ", c(strataDifferences[i]), "\n"))
}

#Compare to Simple Random Sampling
cat("\n-------Comparing Means between Sampling Techniques (Rand Sample - Strata Sample) -----\n");
for (i in seq(from=1, to=5, by=1)) {
  cat(paste("\tIteration", c(i), ":", " Difference => ", c(sampleMeans[i] - strataMeans[i]), "\n"));
}