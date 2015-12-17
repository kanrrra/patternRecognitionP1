library(foreach)  
library(doParallel)  
library(parallel)

numCores <- detectCores()  
cl <- makeCluster(numCores)  
registerDoParallel(cl)


#read the data
digits <- read.table("mnist.csv",header=TRUE, sep = ",")
#digits[, 1] <- as.factor(digits[,1])

#digits <- digits[1:3000,]


#create the training/test sample
smp_size <- 1000
set.seed(123456)
train_ind <- sample(seq_len(nrow(digits)), size = smp_size)

digits.training <- digits[train_ind, ]

nofFeatures <- (dim(digits)[2] -  1)

templates <- matrix(0, nrow=10, ncol=nofFeatures)

test <- aggregate(digits.training, by=list(digits.training[,1]), FUN=mean)
for(i in 3:786){
  templates[,(i-2)] <- as.matrix(test[i])
}

#size of the hists
size <- sqrt(dim(digits)[2] - 1)

features <- digits[,1:(11 + size * 2)]

print("startin parfor")
foreach(i = 1:dim(digits)[1]) %dopar% {
#for(i in 1:dim(digits)[1]){
  print(i)
  currentEntry <- digits[i,-1]
  for(templateLab in 1:10){
    delta <- currentEntry - templates[templateLab,]
    SAD <- sum(abs(delta))
    features[i, (templateLab+1)] <- SAD
  }
}

#hist features
for(i in 1:dim(digits)[1]){
  #progress
  if(i %% 100 == 0){
    cat(i, dim(digits)[1], "\n")
  }
  
  localValue <- as.numeric(digits[i, 2:dim(digits)[2]])
  
  feature <- rep(0, size * 2 + 1)
  histx <- rep(0, size)
  histy <- rep(0, size)
  
  for(j in 1:length(localValue)){
    col <- 1 + (j - 1) %% size
    row <- 1 + (floor((j - 1) / size)) %% size
    
    histx[col] <- histx[col] + localValue[j]
    histy[row] <- histy[row] + localValue[j]
  }
  
  for(j in 1:size){
    features[i, 11 + j] <- histx[j]
    features[i, 11 + size + j] <- histy[j]
  }
}

features.training <- features[train_ind, ]
features.testing <- features[-train_ind, ]