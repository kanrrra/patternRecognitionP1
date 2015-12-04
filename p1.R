#V1 and v40 are useless, they dont contain any data


library(MASS)
library(e1071)
library(nnet)
library(glmnet)

require(ggplot2)

#read the data
opt_digits.dat <- read.table("mnist.csv",header=TRUE, sep = ",")
opt_digits.dat[, 1] <- as.factor(opt_digits.dat[,1])

opt_digits.dat <- opt_digits.dat[1:3000,]


#create the training/test sample
smp_size <- 1000
set.seed(123456)
train_ind <- sample(seq_len(nrow(opt_digits.dat)), size = smp_size)

#simple ink model
ink.dat <- opt_digits.dat[, 1:2]
ink.dat[,2] <- rowSums(opt_digits.dat[,2:length(opt_digits.dat)])

#scale the ink to 0-1 (kinda)
ink.dat[,2] <- scale(ink.dat[,2], FALSE, TRUE)

ink.training <- ink.dat[train_ind, ]
ink.testing <- ink.dat[-train_ind, ]

#learn the multinom model
opt_digits.multinom <- multinom(formula = label ~ ., data = ink.training)
opt_digits.multinom.pred <- predict(opt_digits.multinom, ink.testing)

#predictions are on the vertical axis
print(table(opt_digits.multinom.pred, ink.testing[,1]))



##hist classification
size <- sqrt(dim(opt_digits.dat)[2] - 1)

histFeatures <- opt_digits.dat[,1:(2*size + 1)]

for(i in 1:dim(opt_digits.dat)[1]){
  #progress
  if(i %% 1000 == 0){
    cat(i, dim(opt_digits.dat)[1], "\n")
  }
  
  localValue <- as.numeric(opt_digits.dat[i, 2:dim(opt_digits.dat)[2]])
  
  feature <- rep(0, size * 2 + 1)
  histx <- rep(0, size)
  histy <- rep(0, size)
  
  histFeatures[i, 1] <- opt_digits.dat[i, 1]
  
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
opt_digits.histdata.multinom <- multinom(formula = label ~ ., data = histFeatures.training)
opt_digits.histdata.multinom.pred <- predict(opt_digits.histdata.multinom, histFeatures.testing)

opt_digits.histdata.multinom.conftable <- table(histFeatures.testing[,1], opt_digits.histdata.multinom.pred)
print(opt_digits.histdata.multinom.conftable)


inkHistFeatures.training <- histFeatures.training
inkHistFeatures.training["ink"] <- ink.training[,2]

inkHistFeatures.testing <- histFeatures.testing
inkHistFeatures.testing["ink"] <- ink.testing[,2]


#learn the multinom ink + hist model
opt_digits.inkHistdata.multinom <- multinom(formula = label ~ ., data = inkHistFeatures.training)
opt_digits.inkHistdata.multinom.pred <- predict(opt_digits.inkHistdata.multinom, inkHistFeatures.testing)

opt_digits.inkHistdata.multinom.conftable <- table(inkHistFeatures.testing[,1], opt_digits.inkHistdata.multinom.pred)
print(opt_digits.inkHistdata.multinom.conftable)

opt_digits.histdata.multinomLasso <- cv.glmnet(as.matrix(histFeatures.training[,-1]), as.matrix(histFeatures.training[,1]), family="multinomial", alpha = 1)
opt_digits.histdata.multinomLasso.pred <- predict(opt_digits.histdata.multnomLasso, as.matrix(histFeatures.testing[,-1]), type = "class")

opt_digits.histdata.multinom.conftable <- table(histFeatures.testing[,1], opt_digits.histdata.multinomLasso.pred[,1])
print(opt_digits.histdata.multinom.conftable)

#opt_digits.svm.tune <- tune.svm(label ~ ., data = opt_digits.dat, gamma=10^(-6:-1), cost=10^(-1:1))
#- best parameters:
#  gamma cost
#  0.001   10
#- best performance: 0.4375159 

# data.training <- opt_digits.dat[train_ind, ]
# data.testing <- opt_digits.dat[-train_ind, ]
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