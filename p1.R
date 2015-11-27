#V1 and v40 are useless, they dont contain any data


library(MASS)
library(e1071)
library(nnet)
library(glmnet)

require(ggplot2)

#create the training/test sample
smp_size <- floor(0.8 * nrow(opt_digits.dat))
set.seed(123456)
train_ind <- sample(seq_len(nrow(opt_digits.dat)), size = smp_size)

#read the data
opt_digits.dat <- read.table("mnist.csv",header=TRUE, sep = ",")
opt_digits.dat[, 1] <- as.factor(opt_digits.dat[,1])

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