#V1 and v40 are useless, they dont contain any data


library(MASS)
library(e1071)
library(nnet)
library(glmnet)

require(ggplot2)

#read the data
digits <- read.table("mnist.csv",header=TRUE, sep = ",")
digits[, 1] <- as.factor(digits[,1])

digits <- digits[1:3000,]


#create the training/test sample
smp_size <- 1000
set.seed(123456)
train_ind <- sample(seq_len(nrow(digits)), size = smp_size)

#simple ink model
ink <- digits[, 1:2]
ink[,2] <- rowSums(digits[,2:length(digits)])

#scale the ink to 0-1 (kinda)
ink[,2] <- scale(ink[,2], FALSE, TRUE)

ink.training <- ink[train_ind, ]
ink.testing <- ink[-train_ind, ]

#learn the multinom model
ink.multinom <- multinom(formula = label ~ ., data = ink.training)
ink.multinom.pred <- predict(ink.multinom, ink.testing)
ink.multinom.confTable <- table(ink.multinom.pred, ink.testing[,1])

#predictions are on the vertical axis
print(ink.multinom.confTable)
sum(diag(ink.multinom.confTable))/nrow(histFeatures.testing)



##hist classification
size <- sqrt(dim(digits)[2] - 1)
histFeatures <- digits[,1:(2*size + 1)]

for(i in 1:dim(digits)[1]){
  #progress
  if(i %% 1000 == 0){
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
    histFeatures[i, 1 + j] <- histx[j]
    histFeatures[i, 1 + size + j] <- histy[j]
  }
}

histFeatures.training <- histFeatures[train_ind, ]
histFeatures.testing <- histFeatures[-train_ind, ]

#learn the multinom model
histFeatures.multinom <- multinom(formula = label ~ ., data = histFeatures.training)
histFeatures.multinom.pred <- predict(histFeatures.multinom, histFeatures.testing)

#show the performance
histFeatures.multinom.conftable <- table(histFeatures.testing[,1], histFeatures.multinom.pred)
print(histFeatures.multinom.conftable)
sum(diag(histFeatures.multinom.conftable))/nrow(histFeatures.testing)


#ink + hist model
inkHistFeatures.training <- histFeatures.training
inkHistFeatures.training["ink"] <- ink.training[,2]

inkHistFeatures.testing <- histFeatures.testing
inkHistFeatures.testing["ink"] <- ink.testing[,2]


#learn the multinom ink + hist model
inkHistFeatures.multinom <- multinom(formula = label ~ ., data = inkHistFeatures.training)
inkHistFeatures.multinom.pred <- predict(inkHistFeatures.multinom, inkHistFeatures.testing)

inkHistFeatures.multinom.conftable <- table(inkHistFeatures.testing[,1], inkHistFeatures.multinom.pred)
print(inkHistFeatures.multinom.conftable)
sum(diag(inkHistFeatures.multinom.conftable))/nrow(inkHistFeatures.testing)

#multinom hist with lassooooooo
#lambda.min = 0.002509179
histFeatures.multinomLasso <- cv.glmnet(as.matrix(histFeatures.training[,-1]), as.matrix(histFeatures.training[,1]), family="multinomial", alpha = 1)
histFeatures.multinomLasso.pred <- predict(histFeatures.multinomLasso, as.matrix(histFeatures.testing[,-1]), type = "class")

histFeatures.multinomLasso.conftable <- table(histFeatures.testing[,1], histFeatures.multinomLasso.pred[,1])
print(histFeatures.multinomLasso.conftable)
sum(diag(histFeatures.multinomLasso.conftable))/nrow(histFeatures.testing)

#opt_digits.svm.tune <- tune.svm(label ~ ., data = digits, gamma=10^(-6:-1), cost=10^(-1:1))
#- best parameters:
#  gamma cost
#  0.001   10
#- best performance: 0.4375159 

# data.training <- digits[train_ind, ]
# data.testing <- digits[-train_ind, ]
# 
# 
# opt_digits.svm <- svm(formula = label ~ ., data = data.training, type = "C-classification")#, gamma = 0.001, cost = 10)
# opt_digits.svm.pred <- predict(opt_digits.svm, data.testing[,2:785])
# 
# conf_table <- table(data.testing[,1], opt_digits.svm.pred)
# conf_table_prob <- conf_table/rowSums(conf_table)
# confusion <- as.data.frame(conf_table_prob)
# plot <- ggplot(confusion)
# 
# plot + geom_tile(aes(x=Var1, y=opt_digits.svm.pred, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_fill_gradient(breaks=seq(from=0, to=1, by=.2)) + labs(fill="Normalized\nFrequency")
# 
# print(table(data.testing[,1], opt_digits.svm.pred))