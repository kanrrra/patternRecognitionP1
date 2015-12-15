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

#learn the nn model
features.nn <- nnet(formula = label ~ ., data = features.training, size = 50, MaxNWts = 100000, maxit = 1000)
features.nn.pred <- predict(features.nn, features.testing, type = "class")

#show the performance
features.nn.conftable <- table(features.testing[,1], features.nn.pred)
print(features.nn.conftable)
sum(diag(features.nn.conftable))/nrow(features.testing)
