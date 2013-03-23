## Calculate the gini index of a given node
gini <- function(b.prob, m.prob) {
  ## TODO: FILL IN gini calculation
  return(0)
}

## Calculate the entropy of a given node
entropy <- function(b.prob, m.prob) {
  ## TODO: Fill in entropy calculation
  ent.calc <- rep(0, length(b.prob))
  # Fix the case where you get 0 log 0
  zero.idx <- which(b.prob == 0 | m.prob == 0)
  ent.calc[zero.idx] <- 0
  return(ent.calc)
}

## Function to evaluate the split index using the criteria
## Note that it should either be gini or entropy
tree.split <- function(data, criteria = 'gini') {
  func <- match.fun(criteria)
  data <- data[order(data[,1]),];
  total.n <- nrow(data)
  total.b <- sum(data$diagnosis == 'B')
  parent.index <- func(total.b/total.n, (total.n-total.b)/total.n)
  
  # count the number of b's on the left side
  left.b <- cumsum(data$diagnosis == 'B')
  left.n <- 1:total.n
  # calculate the probability for b and m classes on left side
  left.b.prob <- left.b/left.n
  left.m.prob <- 1- left.b.prob
  left.split <- func(left.b.prob, left.m.prob)
  
  # count the number of b's on the right side
  right.b <- total.b - left.b
  right.n <- total.n - left.n
  
  # calculate the probability for b and m classes on right side
  right.b.prob <- right.b/right.n
  right.m.prob <- 1- right.b.prob
  right.split <- func(right.b.prob, right.m.prob)
  
  ## TODO: Fill in the calculation into index
  child.index <- 0
  
  return(data.frame(split=data[,1], delta.index=parent.index-child.index))
}
