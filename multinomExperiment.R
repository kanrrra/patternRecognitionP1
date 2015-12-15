multinomExperiment <- function(features.training, features.testing){
  library(nnet)
  
  multinom_model <- multinom(formula = label ~ ., data = features.training, MaxNWts = 10000)
  
  multinom.pred <- predict(multinom_model, features.testing)
  
  #results
  confTable <- table(multinom.pred, digits.testing[,1])
  precision <- sum(diag(confTable))/nrow(features.testing)
  
  print(confTable)
  print(precision)
  
  result <- list(model = multinom_model, pred = multinom.pred, conftable = confTable, precision = precision)
}

#read the data
digits <- read.table("mnist.csv",header=TRUE, sep = ",")
digits[, 1] <- as.factor(digits[,1])

digits <- digits[1:3000,]


#create the training/test sample
smp_size <- 1000
set.seed(123456)
train_ind <- sample(seq_len(nrow(digits)), size = smp_size)

digits.training <- digits[train_ind,]
digits.testing <- digits[-train_ind,]

test <- multinomExperiment(digits.training, digits.testing)