# Operations on colleges.txt

# Loading the colleges table
colleges <- read.delim("~/development/data_mining/colleges.txt")

# Removing the categorical data college name
colleges = colleges[,-1]

# Removing rows without data
colleges = na.omit(colleges)
normalized_colleges <- data.frame(nrow(colleges), ncol(colleges)) 

# Normalizing the data
for (i in seq(from=1, to=ncol(colleges), by=1))
  for(j in seq(from=1, to=nrow(colleges), by=1))
    normalized_colleges[j,i] <- (colleges[j,i] - mean(colleges[,i])) / sd(colleges[,i])

for (i in seq(from=1, to=ncol(colleges), by=1))
  for(j in seq(from=1, to=nrow(colleges), by=1))
    colleges[j,i] <- normalized_colleges[j,i]

# Run PCA
colleges.pca <- princomp(colleges)

# View Summary
summary(colleges.pca)

# View Individual Variable Contributions
loadings(colleges.pca)