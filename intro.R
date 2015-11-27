#V1 and v40 are useless, they dont contain any data


library(MASS)
library(e1071)

require(ggplot2)

opt_digits.dat <- read.table("optdigits-tra.txt",header=FALSE, sep = ",")

opt_digits.model <- lm(formula = V65 ~ ., data = opt_digits.dat)
#opt_digits.step <- stepAIC(opt_digits.model, scope=list(lower = ~1, upper = ~.^2), k = log(546))

opt_digits.pred <- predict.lm(opt_digits.model)

print(table(opt_digits.dat[,65] ,round(opt_digits.pred)))

#opt_digits.svm.tune <- tune.svm(V65 ~ ., data = opt_digits.dat, gamma=10^(-6:-1), cost=10^(-1:1))
#- best parameters:
#  gamma cost
#  0.001   10
#- best performance: 0.4375159 

smp_size <- floor(0.2 * nrow(opt_digits.dat))
set.seed(12345)
train_ind <- sample(seq_len(nrow(opt_digits.dat)), size = smp_size)
data.training <- opt_digits.dat[train_ind, ]
data.testing <- opt_digits.dat[-train_ind, ]

opt_digits.svm <- svm(formula = V65 ~ ., data = data.training, type = "C-classification", gamma = 0.001, cost = 10)
opt_digits.svm.pred <- predict(opt_digits.svm, data.testing)

conf_table <- table(data.testing[,65], opt_digits.svm.pred)
conf_table_prob <- conf_table/rowSums(conf_table)
confusion <- as.data.frame(conf_table_prob)
plot <- ggplot(confusion)

plot + geom_tile(aes(x=Var1, y=opt_digits.svm.pred, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_fill_gradient(breaks=seq(from=0, to=1, by=.2)) + labs(fill="Normalized\nFrequency")

print(table(data.testing[,65], opt_digits.svm.pred))