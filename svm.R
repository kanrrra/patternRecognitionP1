#V1 and v40 are useless, they dont contain any data


library(MASS)
library(e1071)
library(nnet)
library(glmnet)


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

#learn the svm model
#svm.tune <- tune.svm(label ~ ., data = features.training, gamma=10^(-6:-1), cost=10^(-1:1))
features.svm <- svm(formula = label ~ ., data = features.training)
features.svm.pred <- predict(features.svm, features.testing)

#show the performance
features.conftable <- table(features.testing[,1], features.svm.pred)
print(features.conftable)
