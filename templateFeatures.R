#read the data
digits <- read.table("mnist.csv",header=TRUE, sep = ",")
#digits[, 1] <- as.factor(digits[,1])

digits <- digits[1:3000,]


#create the training/test sample
smp_size <- 1000
set.seed(123456)
train_ind <- sample(seq_len(nrow(digits)), size = smp_size)

digits.training <- digits[train_ind, ]
digits.testing <- digits[-train_ind, ]

nofRows <- (dim(digits.training)[2] -  1)

templates <- matrix(0, nrow=10, ncol=nofRows)

#for(i in 1:dim(digits.training)[1]){
#  classLab <- digits.training[i, 1]
#  templates[classLab,] <- templates[classLab, ] + digits.training[i, 2:(nofRows + 1)]
#}
  
test <- aggregate(digits.training, by=list(digits.training[,1]), FUN=mean)
for(i in 3:786){
  templates[,(i-2)] <- as.matrix(test[i])
}

features.training <- digits.training[,1:11]
features.testing <- digits.testing[,1:11]

for(i in 1:dim(digits.training)[1]){
  print(i)
  currentEntry <- digits.training[i,-1]
  for(classLab in 1:10){
    delta <- currentEntry - templates[classLab,]
    SAD <- sum(abs(delta))
    features.training[i, (classLab+1)] <- SAD
  }
}

for(i in 1:dim(digits.testing)[1]){
  print(i)
  currentEntry <- digits.testing[i,-1]
  for(classLab in 1:10){
    delta <- currentEntry - templates[classLab,]
    SAD <- sum(abs(delta))
    features.testing[i, (classLab+1)] <- SAD
  }
}

