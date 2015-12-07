#V1 and v40 are useless, they dont contain any data


library(MASS)
library(e1071)
library(nnet)
library(glmnet)

library(class)


#read the data
digits <- read.table("mnist.csv",header=TRUE, sep = ",")
digits[, 1] <- as.factor(digits[,1])

digits <- digits[1:3000,]


#create the training/test sample
smp_size <- 1000
set.seed(123456)
train_ind <- sample(seq_len(nrow(digits)), size = smp_size)

#knn
features.training <- digits[train_ind, ]
features.testing <- digits[-train_ind, ]

#learn the multinom model
features.knn.pred <- knn(train = features.training[,-1], test = features.testing[,-1], cl = features.training[,1], k = 5)
#features.knn.pred <- predict(features.knn, features.testing)

#show the performance
features.knn.conftable <- table(features.testing[,1], features.knn.pred)
print(features.knn.conftable)
