#Initializing fields
library(ggplot2)
bostonHousing = read.table(file="boston-housing.txt", header=TRUE, sep="")
lstat_vec = bostonHousing$LSTAT
lstats = data.frame(bostonHousing$LSTAT)
medv_vec = bostonHousing$MEDV
medvs = data.frame(bostonHousing$MEDV)
lstat_q1 = boxplot(lstat_vec)$stats[2,]
lstat_q3 = boxplot(lstat_vec)$stats[4,]
lstat_iqr = lstat_q3 - lstat_q1
medv_q1 = boxplot(medv_vec)$stats[2,]
medv_q3 = boxplot(medv_vec)$stats[4,]
medv_iqr = medv_q3 - medv_q1

#Creating lstat boxplot
cat("Creating lstat boxplot....\n\n")
ggplot(lstats, aes(lstat, bostonHousing$LSTAT)) + geom_boxplot(outlier.colour="red")

#Viewing lstat outliers
cat("Lstat outliers\n")
print(boxplot(lstat_vec)$out)
cat("\n")

#Calculating cutoff for outliers
cat("lstat outlier cutoffs\n")
cat(paste("\tcutoff 1 - ", (lstat_q1 - lstat_iqr * 1.5), "\n"))
cat(paste("\tcutoff 2 - ", (lstat_q3 + lstat_iqr * 1.5), "\n\n"))

#Creating medv boxplot
cat("Creating medv boxplot....\n\n")
ggplot(medvs, aes(medv, bostonHousing$MEDV)) + geom_boxplot(outlier.colour="red")

#Viewing medv outliers
cat("Medv outliers\n")
print(boxplot(medv_vec)$out)
cat("\n")

#Calculating cutoff for outliers
cat("medv outlier cutoffs\n")
cat(paste("\tcutoff 1 - ", (medv_q1 - medv_iqr * 1.5), "\n"))
cat(paste("\tcutoff 2 - ", (medv_q3 + medv_iqr * 1.5), "\n\n"))

#Creating Scatterplot
cat("Creating scatter plot...\n")
ggplot(bostonHousing, aes(bostonHousing$LSTAT, bostonHousing$MEDV)) + geom_point()